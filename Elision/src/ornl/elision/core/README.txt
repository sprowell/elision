About This Package
==================

If you are planning to change anything in this package, read this to better
understand how it is organized.  THIS IS IMPORTANT!

Elision divides construction from evaluation.  That is, construction (making
the objects that constitute the basic structure of an atom) is distinct from
evaluating these structures (replacing them with a new, equivalent atom based
on semantics).

As an example consider Apply(fun, arg), where fun is a function and arg is an
argument.  Construction yields the structure Apply(fun, arg) and does nothing
else.  Evaluation will use knowledge of the semantics of the function fun to
transform this into fun(arg), which may be a completely different atom.

Actually constructing Apply(fun, arg) is thus potentially wasteful.  If we
really want fun(arg), then why not just immediately construct fun(arg), and
not bother with Apply(fun, arg)?  In fact, this is what Elision does, but
conflating the two concepts can cause trouble because these are actually
different concepts, different concerns, and require different information.

Summary: Construction and evaluation are different things.

Construction is always the job - the only job - of the constructor of an atom
class.  Constructors may check their arguments to determine if they make
sense, but ultimately they must simply construct the required atom.  That is,
new Apply(fun, arg) makes Apply(fun, arg), and does not try to compute
fun(arg).

Evaluation is the job of an Evaluator instance.  This class has methods that
make an atom and perform any necessary evaluation.  Thus if one calls
makeApply(fun, arg) this may construct and return fun(arg) instead of
Apply(fun, arg).  That is, evaluation is job of an Evaluator instance.

Summary: Constructors construct, Evaluators evaluate.

The user of Elision probably doesn't care.  If they write Apply(fun, arg)
then they probably expect fun(arg) to be constructed and returned.  Elision
should avoid making Apply(fun, arg) if fun(arg) is something else.  The
user doesn't want to have to know about the Evaluator.

Likewise, a basic principle in Elision is that everything that can be made
can be "round tripped."  That is, everything valid that can be created
programmatically can be written in the syntax, and vice-versa.  This means
the end user should *not* use the constructors, as it can result in creating
Apply(fun, arg) instead of fun(arg), failing the round trip requirement.  For
this reason, constructors are protected and restricted to the elision package
hierarchy.

Summary: Constructors are always marked protected[elision].

The end user does not want to have to use the Evaluator directly, either.  If
an apply can be matched as "case Apply(op, arg)", then the end user should be
able to create it via "Apply(op, arg)".  To allow this every atom class needs a
companion object.  That companion object must have unapply and apply methods.
The apply method takes an implicit Evaluator instance and invokes the correct
method of the Evaluator.  This means that the Evaluator and the atom classes
are co-dependent - they are in a large tangle.  This is accepted as the price
of simplifying the interface.

Now construction and matching are nicely symmetric.  The user cannot
accidentally use the constructor to avoid this, since constructors are
protected.  The user does not have to think about the methods of the Evaluator,
since they are implicitly used by the companion object's apply method - but
an Evaluator instance must be available!

Summary: The companion object's apply requires an implicit Evaluator.

So far this is the story.  The user wants to make Apply(fun, arg).  They
invoke Apply.apply(fun, arg) (or the equivalent) with an implicit or explicit
Evaluator instance.  This invokes the newApply(fun, arg) method of the
Evaluator instance, and that performs evaluation, deciding to construct
fun(arg).  This then invokes the constructor of the correct class to make the
object that gets returned.

Now the story gets more complicated.  Atoms have operations that the perform.

So that is the structure of the system.  Please make sure to pay attention to
the above and do not break things!
