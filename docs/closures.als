open util/relation

sig Binding {
	definedIn: one Context,
	bindsFunction: lone FunctionContext
} {
	/** Up to 1 binding per function */
	all ctx: FunctionContext | lone ctx.(~@bindsFunction)

	/** A binding can only bind a function in its own level */
	bindsFunction in definedIn.(~createdIn)
}

sig FunctionDefinition extends Binding {} {
	one bindsFunction

	this in bindsFunction.captures
}

abstract sig Context {}
one sig TopContext extends Context {}
sig FunctionContext extends Context {
	createdIn: one Context,
	captures: set Binding
} {
	acyclic[@createdIn, Context]

	/** Captures some subset of the bindings that appear in the parent */
	captures in bindingsInCtx[createdIn]

	/** If the parent captures, then this definitely does too */
	createdIn.@captures in captures
}

fun bindingsInCtx[ctx: Context] : set Binding {
	ctx.(~definedIn) + ctx.captures
}

/** Recursion must be possible for named functions */
selfCapture: check {
	all funCtx: FunctionContext, binding: Binding {
		// Every function captures it own binding, if it is defined with a function definition
		binding->funCtx in bindsFunction && binding in FunctionDefinition
		=> funCtx->binding in captures
	}
} for 20

refCycle: check {
	acyclic[bindsFunction+captures, Context+Binding]
} for 20

example: run { 
	#TopContext.(~createdIn) = 2
} for 10

