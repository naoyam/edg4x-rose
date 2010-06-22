#pragma once

#include "rose.h"
#include <boost/tuple/tuple.hpp>

typedef bool SynthetizedAttribute;

/** Stores a function call expression, along with associated information about its context. */
struct FunctionCallInfo
{
	/** The function call expression. */
	SgFunctionCallExp* functionCall;

	/** When a variable is created to replace one of the arguments of this function, where should it be inserted?
	  * The declaration of the variable will occur right before this statement. */
	SgStatement* tempVarDeclarationLocation;

	/** How a statement should be inserted.   */
	enum InsertionMode 
	{
		/** Insert right before the given statement. */
		INSERT_BEFORE,
		/** Insert at the bottom of the scope defined by the given statement. */
		APPEND_SCOPE,
		INVALID
	};

	/** How to insert the temporary variable declaration. */
	InsertionMode tempVarDeclarationInsertionMode;

	/** Should the temporary variable be initialized in the same place it's declared? E.g. for the increment
	 * part of a for loop, the declaration should go outside the for loop, but the evaluation should be inside. */
	bool initializeTempVarAtDeclaration;

	/** Location where to generate an assignment from the argument expression to the temporary variable replacing it.
	  * Could be NULL if the temporary variable is initialized in its declaration. */
	SgStatement* tempVarEvaluationLocation;

	/** How to insert the evaluation. */
	InsertionMode tempVarEvaluationInsertionMode;

	FunctionCallInfo(SgFunctionCallExp* function) : 
		functionCall(function),
		tempVarDeclarationLocation(NULL),
		tempVarDeclarationInsertionMode(INVALID),
		initializeTempVarAtDeclaration(NULL),  
		tempVarEvaluationLocation(NULL),
		tempVarEvaluationInsertionMode(INVALID)
		{}
};


struct FunctionCallInheritedAttribute
{
	/** The innermost loop inside of which this AST node resides. It is either a for-loop,
	 a do-looop, or a while-loop. */
	SgScopeStatement* currentLoop;

	/** The last statement encountered before the current node in the AST. */
	SgStatement* lastStatement;

	/** Is the current node inside a for loop structure (not the body). */
	enum { INSIDE_FOR_INIT, INSIDE_FOR_TEST, INSIDE_FOR_INCREMENT, INSIDE_WHILE_CONDITION,
			INSIDE_DO_WHILE_CONDITION, NOT_IN_LOOP }
	loopStatus;

	/** Default constructor. Initializes everything to NULL. */
	FunctionCallInheritedAttribute() : currentLoop(NULL), lastStatement(NULL), loopStatus(NOT_IN_LOOP) {}
};


class FunctionEvaluationOrderTraversal : public AstTopDownBottomUpProcessing<FunctionCallInheritedAttribute, SynthetizedAttribute>
{
public:
	/** Traverses the subtree of the given AST node and finds all function calls in
	 * function-evaluation order. */
	static std::vector<FunctionCallInfo> GetFunctionCalls(SgNode* root);

	/** Visits AST nodes in pre-order */
	FunctionCallInheritedAttribute evaluateInheritedAttribute(SgNode* astNode, FunctionCallInheritedAttribute parentAttribute);

	/** Visits AST nodes in post-order. This is function-evaluation order. */
	SynthetizedAttribute evaluateSynthesizedAttribute(SgNode* astNode, FunctionCallInheritedAttribute parentAttribute, SynthesizedAttributesList);

private:

	FunctionEvaluationOrderTraversal() {}

	/** All the function calls seen so far. */
	std::vector<FunctionCallInfo> functionCalls;
};

class ExtractFunctionArguments
{
public:

    /** Perform the function argument extraction on all function calls in the given subtree of the AST. */
	void NormalizeTree(SgNode* tree);

private:

	/** Given the expression which is the argument to a function call, returns true if that
	  * expression should be pulled out into a temporary variable on a separate line.
	  * E.g. if the expression contains a function call, it needs to be normalized, while if it
	  * is a constant, there is no need to change it. */
	bool FunctionArgumentNeedsNormalization(SgExpression* argument);

	/** Returns true if any of the arguments of the given function call will need to
	  * be extracted. */
	bool FunctionCallNeedsNormalization(SgFunctionCallExp* functionCall);

	/** Returns true if any function calls in the given subtree will need to be
	  * instrumented. (to extract function arguments). */
	bool SubtreeNeedsNormalization(SgNode* top);

	/** Given the information about a function call (obtained through a traversal), extract its arguments
	  * into temporary variables where it is necessary. */
	void RewriteFunctionCallArguments(const FunctionCallInfo& functionCallInfo);

	/** Given an expression, generates a temporary variable whose initializer optionally evaluates
	  * that expression. Then, the var reference expression returned can be used instead of the original
	  * expression. The temporary variable created can be reassigned to the expression by the returned SgAssignOp;
	  * this can be used when the expression the variable represents needs to be evaluated. NOTE: This handles
	  * reference types correctly by using pointer types for the temporary.
	  * @param expression Expression which will be replaced by a variable
	  * @param scope scope in which the temporary variable will be generated
	  * @return declaration of the temporary variable, an assignment op to
	  *			reevaluate the expression, and a a variable reference expression to use instead of
	  *         the original expression. Delete the results that you don't need! */
	static boost::tuple<SgVariableDeclaration*, SgAssignOp*, SgExpression* > CreateTempVariableForExpression(SgExpression* expression,
		SgScopeStatement* scope, bool initializeInDeclaration);

	/** Take a statement that is located somewhere inside the for loop and move it right before the
	  * for looop. If the statement is a variable declaration, the declaration is left in its original
	  * location to preserve its scope, and a new temporary variable is introduced. */
	void HoistStatementOutsideOfForLoop(SgForStatement* forLoop, SgStatement* statement);

	/** Generate a name that is unique in the current scope and any parent and children scopes.
	  * @param baseName the word to be included in the variable names. */
	static std::string GenerateUniqueVariableName(SgScopeStatement* scope, std::string baseName = "temp");

	/** Insert a new statement in the specified location. The actual insertion can occur either before or after the location
	  * depending on the insertion mode. */
	void InsertStatement(SgStatement* newStatement, SgStatement* location, FunctionCallInfo::InsertionMode insertionMode);
};
