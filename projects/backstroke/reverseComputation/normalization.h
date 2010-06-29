#ifndef REVERSE_COMPUTATION_NORMALIZATION
#define REVERSE_COMPUTATION_NORMALIZATION

#include <rose.h>
#include <boost/foreach.hpp>

#define foreach BOOST_FOREACH


using namespace std;
using namespace boost;
using namespace SageBuilder;
using namespace SageInterface;


bool isAssignmentOp(SgExpression* e);

vector<SgExpression*> getAndReplaceModifyingExpression(SgExpression* exp);

SgExpression* normalizeExpression(SgExpression* exp);

void splitCommaOpExp(SgExpression* exp);

void removeUselessBraces(SgBasicBlock* body);

#endif
