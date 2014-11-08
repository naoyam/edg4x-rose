/* accDirectives.cpp */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

// This is derived from "OmpAttribute.C" and "ompAstConstruction.cpp"
// by Liao, 10/27/2008.  See comments in them.

// RESTRICTIONS: (0) Making an ACC block may fail in counting affected
// blocks (in accHuntForBlock()), because it does not skip
// non-executable nodes such as pragma nodes.

// MEMO (MINOR): (0) Continued directive lines are concatenated and
// unparser output has reduced number of lines.  (1) No unparsers are
// defined for ACC directives and clauses, since they are subclasses
// of SgLocatedNodeSupport (they return empty string).  They are
// unparsed via ACC blocks.  (0) AccBlock should better be divided
// into two directive types, one of which affects blocks and the other
// has executable parts.  (One be a subclass of SgBasicBlock and the
// other be a subclass of SgExprStatement).  (1) Adding (pragma) nodes
// after a block fails when the block is a body of a function
// definition (a block is of type basic-block).  (2) The type
// AttachedPreprocessingInfoType is vector<PreprocessingInfo*>.

#include <iostream>
#include <vector>

#include "sage3basic.h"
#include "rose_paths.h"
#include "astPostProcessing.h"
#include "sageBuilder.h"
#include "AstSimpleProcessing.h"

#include "sageInterface.h"
#ifndef ROSE_USE_INTERNAL_FRONTEND_DEVELOPMENT
#include "CallGraph.h"
#endif

#include "accDirectives.h"

namespace AccSupport
{

#define VERBOSE2PRINT(...) \
  if (SgProject::get_verbose() > 1) {printf(__VA_ARGS__);}

  // Returns a target (affected) statement of a directive: ACC_SELF
  // for nothing, ACC_NEXT for the next statement, and ACC_F_BLOCK for
  // the next statement in C and the next block in Fortran.

  enum acc_effected accEffected(enum SgAccDirective::acc_directive e) {
    switch (e) {
      // Directives (2.5)

    case e_acc_parallel:
      return acc_f_block;
    case e_acc_kernels:
      return acc_f_block;
    case e_acc_data:
      return acc_f_block;
    case e_acc_enter_data:
      return acc_self;
    case e_acc_exit_data:
      return acc_self;
    case e_acc_host_data:
      return acc_f_block;

      // Constructs (2.6.6, 2.7)

    case e_acc_loop:
      return acc_next;
    case e_acc_parallel_loop:
      // (with optional end)
      return acc_next;
    case e_acc_kernels_loop:
      // (with optional end)
      return acc_next;
    case e_acc_cache:
      return acc_self;

    case e_acc_atomic_update:
      // (with optional end)
      return acc_f_block;
    case e_acc_atomic_read:
      // (with optional end)
      return acc_f_block;
    case e_acc_atomic_write:
      // (with optional end)
      return acc_f_block;
    case e_acc_atomic_capture:
      return acc_f_block;

    case e_acc_declare:
      return acc_self;

      // Executable Directives (2.12)

    case e_acc_update:
      return acc_self;
    case e_acc_routine:
      return acc_self;
    case e_acc_wait:
      return acc_self;

      // Fortran Block End (dummy entries)

    case e_acc_end_parallel:
      return acc_self;
    case e_acc_end_kernels:
      return acc_self;
    case e_acc_end_data:
      return acc_self;
    case e_acc_end_host_data:
      return acc_self;
    case e_acc_end_atomic:
      return acc_self;
    };
    assert(0);
    return acc_self;
  }

  // Returns a corresponding end block marker (in Fortran).

  enum SgAccDirective::acc_directive accBlockClosing(
    enum SgAccDirective::acc_directive e) {
    enum SgAccDirective::acc_directive none = e_acc_parallel;
    switch (e) {
      // Directives (2.5)

    case e_acc_parallel:
      return e_acc_end_parallel;
    case e_acc_kernels:
      return e_acc_end_kernels;
    case e_acc_data:
      return e_acc_end_data;
    case e_acc_enter_data:
      return none;
    case e_acc_exit_data:
      return none;
    case e_acc_host_data:
      return e_acc_end_host_data;

      // Constructs (2.6.6, 2.7)

    case e_acc_loop:
      return none;
    case e_acc_parallel_loop:
      // (with optional end)
      return none;
    case e_acc_kernels_loop:
      // (with optional end)
      return none;
    case e_acc_cache:
      return none;

    case e_acc_atomic_update:
      // (with optional end)
      return e_acc_end_atomic;
    case e_acc_atomic_read:
      // (with optional end)
      return e_acc_end_atomic;
    case e_acc_atomic_write:
      // (with optional end)
      return e_acc_end_atomic;
    case e_acc_atomic_capture:
      return e_acc_end_atomic;

    case e_acc_declare:
      return none;

      // Executable Directives (2.12)

    case e_acc_update:
      return none;
    case e_acc_routine:
      return none;
    case e_acc_wait:
      return none;

      // Fortran Block End (dummy entries)

    case e_acc_end_parallel:
      return none;
    case e_acc_end_kernels:
      return none;
    case e_acc_end_data:
      return none;
    case e_acc_end_host_data:
      return none;
    case e_acc_end_atomic:
      return none;
    };
    assert(0);
    return none;
  }

  void accTakeoutDirectives(SgSourceFile *file);
  void accTakeoutDirectivesInNode(SgLocatedNode* node, SgSourceFile *file);
  void accParseDirectives(SgSourceFile *file);
  void accParseDirectiveNode(SgPragmaDeclaration* dir, SgSourceFile *file);

  // Trims off "!..." pattern from both ends of the string and leave
  // only "!$omp..." or "!$acc...".  (This is a copy of
  // "removeFortranComments()" in "ompFortranParser.C").

  std::string accTrimNondirectives(std::string &s, std::string ompacc) {
    size_t keylen = ompacc.size();
    size_t beg = std::string::npos;
    size_t end = s.size();
    for (size_t p = s.find("!", 0);
         p != std::string::npos; p = s.find("!", (p + 1))) {
      if (s.compare(p, keylen, ompacc) == 0) {
        if (beg != std::string::npos) {
          std::cerr << "ACC: error: Multiple pragmas in a line" << std::endl;
          ROSE_ASSERT(0);
        }
        beg = p;
      } else if (beg != std::string::npos && p < end) {
        assert(beg < p);
        end = p;
      }
    }
    if (beg == std::string::npos) {
      beg = end;
    }
    end = (s.find_last_not_of(" \t", end) + 1);
    return s.substr(beg, (end - beg));
  }

  // Destructively appends string S to string DIREC, dropping two "&".

  void accAppendContinuedDirective(std::string &direc, std::string s) {
    if (direc == "") {
      direc = s;
    } else {
      assert(s.substr(0, 5) == "!$acc");
      size_t pos0 = s.find_first_not_of(" \t", 5);
      if (pos0 == std::string::npos || s.at(pos0) != '&') {
        std::cerr << "ACC: error: Bad pragma continuation line" << std::endl;
        ROSE_ASSERT(0);
      }
      /*size_t pos1 = s.find_first_not_of(" \t", (pos0 + 1));*/
      direc.resize(direc.size() - 1);
      direc += s.substr((pos0 + 1), std::string::npos);
    }
  }

  /* Checks SgLocatedNodes are ordered as appearances in a file
     (is_sorted is C++11). */

  bool accCheckNodesOrdering(std::vector<SgNode*> *vv) {
    Sg_File_Info* prev = NULL;
    for (std::vector<SgNode*>::iterator
           i = vv->begin(); i != vv->end(); i++) {
      SgLocatedNode* n = isSgLocatedNode(*i);
      assert(n != NULL);
      Sg_File_Info* p = n->get_startOfConstruct();
      if (prev != NULL) {
        if (!(prev->get_line() < p->get_line()
              || ((prev->get_line() == p->get_line()
                   && prev->get_col() <= p->get_col())))) {
          return false;
        }
      }
      prev = p;
    }
    return true;
  }

  //! Extracts directive lines from comments, and inserts new pragma
  // nodes before a node.  It is only necessary in Fortran.  It is
  // called before processOpenMP() in secondaryPassOverSourceFile() in
  // "sage_support.cpp".  See
  // convert_Fortran_OMP_Comments_to_Pragmas() and
  // parse_fortran_openmp().  MEMO: Use of
  // SageInterface::insertStatementList() is avoided, because it
  // copies comments (no options not to copy them).

  void accTakeoutDirectives(SgSourceFile *file) {
    /* (Trival Check: fortran subsumes sublanguages.) */
    assert(file->get_Fortran_only()
           || !(file->get_F77_only()
                || file->get_F90_only()
                || file->get_F95_only()
                || file->get_F2003_only()));

    if (!file->get_acc()) {
      return;
    }

    VERBOSE2PRINT("ACC: processing...\n");

    std::vector<SgNode*>
      nodes = NodeQuery::querySubTree(file, V_SgLocatedNode);
    for (std::vector<SgNode*>::iterator
           i = nodes.begin(); i != nodes.end(); i++) {
      SgLocatedNode* node = isSgLocatedNode(*i);
      ROSE_ASSERT(node != NULL);
      accTakeoutDirectivesInNode(node, file);
    }
  }

  // Extracts directive lines from comments.  It moves remaining
  // (non-directive) comments to created pragams.  Comments at the end
  // of a block which are attached at "inside", are moved along with a
  // pragma and attached at "before".

  void accTakeoutDirectivesInNode(SgLocatedNode* onode, SgSourceFile *file) {
    const enum PreprocessingInfo::DirectiveType
      FortranStyleComment = PreprocessingInfo::FortranStyleComment;
    SgNode* parent = onode->get_parent();
    std::vector<SgStatement*> pragmas;
    std::vector<PreprocessingInfo*> others;
    std::string line = "";
    PreprocessingInfo* pos[2] = {NULL, NULL};
    PreprocessingInfo::RelativePositionType pploc = PreprocessingInfo::undef;

    std::vector<PreprocessingInfo*>*
      pplines = onode->get_attachedPreprocessingInfoPtr();
    if (pplines == NULL) {
      return;
    }

    for (std::vector<PreprocessingInfo*>::iterator
           i = pplines->begin(); i != pplines->end(); i++) {
      PreprocessingInfo* p = (*i);
      /*p->display("ACCACC");*/

      if (!(p->getNumberOfLines() == 1
            && p->getTypeOfDirective() == FortranStyleComment)) {
        // Skip non-comments.
        others.push_back(p);
        continue;
      }

      std::string s = p->getString();
      std::transform(s.begin(), s.end(), s.begin(), ::tolower);
      std::string d = accTrimNondirectives(s, "!$acc");

      if (d == "") {
        // Skip non-directive comments.
        if (line != "") {
          std::cerr << "ACC: error: Comment after a continued directive"
                    << std::endl;
          ROSE_ASSERT(0);
        }
        others.push_back(p);
        continue;
      }

      // See a directive line.

      accAppendContinuedDirective(line, d);
      if (pos[0] == NULL) {
        pos[0] = p;
      }
      pos[1] = p;

      if (pploc == PreprocessingInfo::undef) {
        pploc = p->getRelativePosition();
      }
      assert(pploc == p->getRelativePosition());

      assert(line.size() != 0 && pos != NULL);
      if (line.at(line.size() - 1) == '&') {
        // Collect continued directive lines.
        continue;
      }

      /*VERBOSE2PRINT("ACC: directive=%s\n", line.c_str());*/

      {
        /* buildPragmaDeclaration() cannot be used here. */

        Sg_File_Info* beg = pos[0]->get_file_info();
        Sg_File_Info* end = new Sg_File_Info(*(pos[1]->get_file_info()));
        assert(pos[1]->getNumberOfLines() == 1);
        end->set_col(pos[1]->getColumnNumberOfEndOfString());

        std::string ss = line.substr(2, std::string::npos);
        SgPragma* p0 = new SgPragma(ss);
        p0->set_startOfConstruct(new Sg_File_Info(*beg));
        p0->set_endOfConstruct(new Sg_File_Info(*end));

        SgPragmaDeclaration* dnode = new SgPragmaDeclaration(p0);
        dnode->set_parent(parent);
        dnode->set_startOfConstruct(new Sg_File_Info(*beg));
        dnode->set_endOfConstruct(new Sg_File_Info(*end));
        p0->set_parent(dnode);
        dnode->set_definingDeclaration(dnode);
        dnode->set_firstNondefiningDeclaration(dnode);

        delete end;

        if (others.size() > 0) {
          switch (pploc) {
          case PreprocessingInfo::defaultValue:
          case PreprocessingInfo::undef:
            break;
          case PreprocessingInfo::before:
            break;
          case PreprocessingInfo::after:
            break;
          case PreprocessingInfo::inside: {
            SgBasicBlock* nodex = isSgBasicBlock(onode);
            if (nodex == NULL) {
              std::cerr << "ACC: error: Unexpected preprocessor lines;"
                        << " Preprocessor lines attached (inside) to: "
                        << onode->class_name() << std::endl;
              ROSE_ASSERT(0);
            }
            for (std::vector<PreprocessingInfo*>::iterator
                   j = others.begin(); j != others.end(); j++) {
              (*j)->setRelativePosition(PreprocessingInfo::before);
            }
            break;
          }
          case PreprocessingInfo::before_syntax:
          case PreprocessingInfo::after_syntax:
            break;
          }
          std::vector<PreprocessingInfo*>*
            pp = new std::vector<PreprocessingInfo*>(others);
          dnode->set_attachedPreprocessingInfoPtr(pp);
          others.clear();
        }

        pragmas.push_back(dnode);
        line = "";
        pos[0] = NULL;
        pos[1] = NULL;
      }
    }
    if (line != "") {
      std::cerr << "ACC: error: Nothing comes after a continued line"
                << std::endl;
      ROSE_ASSERT(0);
    }

    // Insert created pragmas.

    if (pragmas.size() > 0) {
      /*PreprocessingInfo::RelativePositionType*/
      switch (pploc) {
      case PreprocessingInfo::defaultValue:
      case PreprocessingInfo::undef:
        std::cerr << "ACC: error: Bad preprocessor line position (undef)"
                  << std::endl;
        ROSE_ASSERT(0);
        break;

      case PreprocessingInfo::before: {
        SgStatement* nodex = isSgStatement(onode);
        if (nodex == NULL) {
          std::cerr << "ACC: error: (internal) Unexpected preprocessor lines;"
               << " Preprocessor lines attached (before) to: "
               << onode->class_name() << std::endl;
          ROSE_ASSERT(0);
        }
        /*SageInterface::insertStatementList(nodex, pragmas, true);*/
        for (std::vector<SgStatement*>::iterator
               k = pragmas.begin(); k != pragmas.end(); k++) {
          SageInterface::insertStatement(nodex, (*k), true, false);
        }
        break;
      }

      case PreprocessingInfo::after:
        std::cerr << "ACC: error: Bad preprocessor line position (after)"
                  << std::endl;
        ROSE_ASSERT(0);
        break;

      case PreprocessingInfo::inside: {
        SgBasicBlock* nodex = isSgBasicBlock(onode);
        if (nodex == NULL) {
          std::cerr << "ACC: error: (internal) Unexpected preprocessor lines;"
               << " Preprocessor lines attached (inside) to: "
               << onode->class_name() << std::endl;
          ROSE_ASSERT(0);
        }
        SageInterface::appendStatementList(pragmas, nodex);
        break;
      }

      case PreprocessingInfo::before_syntax:
      case PreprocessingInfo::after_syntax:
        std::cerr << "ACC: error: Bad preprocessor line position"
             << " (before/after_syntax)" << std::endl;
        ROSE_ASSERT(0);
        break;
      }

      // Consume comment lines, leave unprocessed ones.

      pplines->clear();
      pplines->assign(others.begin(), others.end());
    }
  }

  // Hunts for a block enclosed by begin-end, for a pragma DNODE in a
  // statement list SEQ.  It returns two positions (iterators)
  // enclosing a block, and its return value indicates if an end
  // marker existed.  It assumes acc-end-pragma nodes are already
  // converted to acc-blockes.  It is somewhat complicated by atomics
  // (except for capture), whose search range is one statement and
  // whose end markers are optional.

  bool accHuntForBlock(SgAccDirective* v, SgPragmaDeclaration* dnode,
                       std::vector<SgStatement*>* seq,
                       SgSourceFile* file,
                       std::vector<SgStatement*>::iterator* begposref,
                       std::vector<SgStatement*>::iterator* endposref) {
    SgAccDirective::acc_directive e = v->get_name();
    enum SgAccDirective::acc_directive closing = accBlockClosing(e);
    std::vector<SgStatement*>::iterator
      dpos = std::find(seq->begin(), seq->end(), dnode);
    if (dpos == seq->end()) {
      std::cerr << "ACC: error: Cannot position an ACC directive" << std::endl;
      ROSE_ASSERT(0);
    }

    std::vector<SgStatement*>::iterator begpos = (dpos + 1);
    std::vector<SgStatement*>::iterator limit;
    switch (e) {
    case e_acc_atomic_update:
    case e_acc_atomic_read:
    case e_acc_atomic_write:
      limit = ((seq->end() - begpos) < 2 ? seq->end() : (begpos + 2));
      break;
    default:
      limit = seq->end();
      break;
    }

    std::vector<SgStatement*>::iterator endpos = seq->end();
    for (std::vector<SgStatement*>::iterator
           i = begpos; i != limit; i++) {
      SgAccBlock* enode = isSgAccBlock(*i);
      if (enode != NULL && enode->get_directive()->get_name() == closing) {
        endpos = i;
        break;
      }
    }
    bool endmark = (endpos != seq->end());
    if (!endmark) {
      switch (e) {
      case e_acc_atomic_update:
      case e_acc_atomic_read:
      case e_acc_atomic_write:
        endpos = (begpos + 1);
        assert(endpos < seq->end());
        break;
      case e_acc_atomic_capture:
      default:
        std::cerr << "ACC: error: Cannot position ACC block end" << std::endl;
        ROSE_ASSERT(0);
        break;
      }
    }

    long count = std::distance(begpos, endpos);
    if (count == 0) {
      std::cerr << "ACC: error: Empty block for ACC directive" << std::endl;
      ROSE_ASSERT(0);
    }

    *begposref = begpos;
    *endposref = endpos;
    return endmark;
  }

  //! Parses directives in pragma nodes and replace them by acc-block
  // nodes.  See convert_OpenMP_pragma_to_AST().  It assumes directive
  // strings are downcased already in Fortran.  It processes nodes in
  // the reverse order of the appearances in a file, intending the
  // post-ordering of the nested directives.

  void accParseDirectives(SgSourceFile *file) {
    ROSE_ASSERT(file != NULL);
    if (!file->get_acc()) {
      return;
    }

    bool langf = file->get_Fortran_only();
    if (!langf) {
      /* This message is already printed in Fortran. */
      VERBOSE2PRINT("ACC: processing...\n");
    }

    std::vector<SgNode*>
      directives = NodeQuery::querySubTree(file, V_SgPragmaDeclaration);

    VERBOSE2PRINT(("ACC: number of directives=%ld;"
                   " processing directives backwards\n"),
                  directives.size());

    ROSE_ASSERT(accCheckNodesOrdering(&directives));

    for (std::vector<SgNode*>::reverse_iterator
           i = directives.rbegin(); i != directives.rend(); i++) {
      SgPragmaDeclaration* dir = isSgPragmaDeclaration(*i);
      ROSE_ASSERT(dir != NULL);
      accParseDirectiveNode(dir, file);
    }

    if (file->get_acc_ast_only()) {
      VERBOSE2PRINT("ACC: skipping lower processing (acc_ast_only)\n");
      return;
    }

    printf("ACC: no semantics processing, yet\n");
  }

  // MEMO: (DELETING OLD NODES SHOULD BE NECESSARY, BUT IT BREAKS).

  void accParseDirectiveNode(SgPragmaDeclaration* dnode, SgSourceFile *file) {
    ROSE_ASSERT(dnode != NULL && file != NULL);
    bool langf = file->get_Fortran_only();

    SageInterface::replaceMacroCallsWithExpandedStrings(dnode);
    std::string s = dnode->get_pragma()->get_pragma();
    std::istringstream is(s);
    std::string key;
    is >> key;
    if (key != "acc") {
      return;
    }

    VERBOSE2PRINT("ACC: parsing line=(%s)\n", s.c_str());

    SgStatement* n = SageInterface::getNextStatement(dnode);
    SgAccDirective* v;
    if (langf) {
      acc_f_parser_init(dnode, n, s.c_str());
      acc_f_parse();
      v = acc_f_parsed_directive();
    } else {
      acc_c_parser_init(dnode, n, s.c_str());
      acc_c_parse();
      v = acc_c_parsed_directive();
    }

    if (v == NULL) {
      // Optional block end (in Fortran).
      SageInterface::removeStatement(dnode, true);
      VERBOSE2PRINT("ACC: adding block=()\n");
      return;
    }

    // Replace a pragma with a new acc-block.

    SgAccBlock* accstmt = new SgAccBlock(v, NULL);
    v->set_parent(accstmt);
    Sg_File_Info* beg = dnode->get_startOfConstruct();
    Sg_File_Info* end = dnode->get_endOfConstruct();
    accstmt->set_startOfConstruct(new Sg_File_Info(*beg));
    accstmt->set_endOfConstruct(new Sg_File_Info(*end));
    /*setOneSourcePositionForTransformation(accstmt);*/

    SgAccDirective::acc_directive e = v->get_name();
    enum acc_effected ee = accEffected(e);
    switch (ee) {
    case acc_self: {
      std::vector<PreprocessingInfo*>*
        pp = dnode->get_attachedPreprocessingInfoPtr();
      accstmt->set_attachedPreprocessingInfoPtr(pp);
      SageInterface::replaceStatement(dnode, accstmt, false);
      /*delete dnode;*/
      break;
    }

    case acc_next:
    case acc_f_block: {
      std::vector<PreprocessingInfo*>*
        pp = dnode->get_attachedPreprocessingInfoPtr();
      accstmt->set_attachedPreprocessingInfoPtr(pp);

      if (!(langf && ee == acc_f_block)) {
        if (n == NULL) {
          std::cerr << "ACC: error: No applicable statements to directive"
                    << std::endl;
          ROSE_ASSERT(n != NULL);
        }
        SgBasicBlock* body;
        if (isSgBasicBlock(n)) {
          body = isSgBasicBlock(n);
          SageInterface::removeStatement(n, false);
        } else {
          body = SageBuilder::buildBasicBlock();
          SgScopeStatement* bodyscope = isSgScopeStatement(body);
          SageInterface::removeStatement(n, false);
          SageInterface::appendStatement(n, bodyscope);
        }
        accstmt->set_body(body);
        body->set_parent(accstmt);
        accstmt->set_parent(dnode->get_parent());
        SageInterface::replaceStatement(dnode, accstmt, false);
        /*delete dnode;*/
      } else {
        SgScopeStatement* scope = dnode->get_scope();
        std::vector<SgStatement*> seq = scope->getStatementList();

        std::vector<SgStatement*>::iterator begpos;
        std::vector<SgStatement*>::iterator endpos;
        bool endp = accHuntForBlock(v, dnode, &seq, file, &begpos, &endpos);
        SgAccBlock* enode;
        if (endp) {
          enode = isSgAccBlock(*endpos);
        } else {
          enode = NULL;
        }

        // Move a block inside a new acc-block.

        SgBasicBlock* body = SageBuilder::buildBasicBlock();
        SgScopeStatement* bodyscope = isSgScopeStatement(body);
        for (std::vector<SgStatement*>::iterator
               j = begpos; j != endpos; j++) {
          SgStatement* s = *j;
          SageInterface::removeStatement(s, false);
          SageInterface::appendStatement(s, bodyscope);
        }
        accstmt->set_body(body);
        body->set_parent(accstmt);
        if (enode != NULL) {
          accstmt->set_closing(enode);
        }
        accstmt->set_parent(dnode->get_parent());
        SageInterface::replaceStatement(dnode, accstmt, false);
        /*SageInterface::removeStatement(dnode, false);*/
        if (enode != NULL) {
          SageInterface::removeStatement(enode, false);
        }
        /*delete dnode;*/
     }
      break;
    }
    }

    VERBOSE2PRINT("ACC: adding block=(%s)\n",
                  accstmt->unparseToString().c_str());
  }

}

#ifndef ROSE_USE_INTERNAL_FRONTEND_DEVELOPMENT

void SgAccBlock::post_construction_initialization() {}

// MEMO: addIncomingFortranGotos(), getNodeJustAfterInContainer(), and
// getNodeJustBeforeInContainer() are in "memberFunctions.C" in
// "src/frontend/SageIII/virtualCFG/".

//unsigned int SgAccBlock::cfgIndexForEnd() const {}
//std::vector<CFGEdge> SgAccBlock::cfgOutEdges(unsigned int i) {}
//std::vector<CFGEdge> SgAccBlock::cfgInEdges(unsigned int idx) {}

#endif //ROSE_USE_INTERNAL_FRONTEND_DEVELOPMENT
