
// Copyright (C) 2020 - 2024 John Steven Matthews

grammar Good; 
    

// =====================  LEXER  =============================
/*


	Good tokens fall into 3 categories: reserved words,
	punctuation, and synthetic tokens.

	Reserved words and punctuation are straight forward
	literal tokens. Synthetic tokens are composed by rule.
	
	The most important synthetic token is an SID (source
	identifier). A SID is a name or reference to

		type 
		object (type instance)
		proxy 
		method 
		subroutine 
		book
		page

	An SID can be a simple name or a complex reference
	composed of several parts.  It's the compiler's job to
	determine the validity of an SID in context.  

	The rule for a simple, unqualified SID matches every
	reserved word, but reserved words have precedence
	(they're listed first), so a simple SID can't be a 
	reserved word.  However, a complex general SID may
	contain embedded reserved words.

	This means that a reserved word won't be returned
	as an SID and can't be used to name new entities (good),
	but it's possible to code a syntactically valid reference
	SID using reserved words that can't possibly reference a
	real entity. But no big deal, because reference validation
	is what compilers do.

	Good has reserved names that are not reserved words.
	"ao" is the reserved assumed object name, and these 
	generic domain names are reserved: @book, @page, @type,
	@this.

	All printable ascii chars are assigned except
	
		$ (dollar sign ) and ` (accent )

	which are reserved for internal compiler use.

	basic identifier...

		a-z A-Z 0-9 _ ~ @ & ? ! 

	punctuation...
	
		''	literal
		""	formula
		[]	attribute list,new analog 
		{}	group
		<>	factor
		()	list, precedence 
		||	bar-operator
		->	proxy assignment (proxy to object)
		.	subobject ref
		\	namespace qualification
		;	group item delimiter
		,	list item separator
		^	literal insert
		=	equivalence, synonomous (operation,nom_type,result)
		#	factor dimension
		:	method call
		+	plus special operator
		-	minus special operator
		*	mul special operator, var input recognition
		/	div special operator

		%	compiler directive

		--		remark
		{{}}	narrative

*/


// reserved words and directives
// trailing underscores avoid collisions in generated C++ code

IGNORE_				:	'%ignore'		;
END					:	'%end'			;

ABSTRACT			:	'abstract'		;
AFTER				:	'after'			;
ALIGN				:	'align'			;
AS					:	'as'			;
BASE				:	'base'			;
COMMON				:	'common'		;	
COMPATIBLE			:	'compatible'	;		
CONST_				:	'const'			;		// constant
ELSE				:	'else'			;
ENUM				:	'enum'			;		// enumerated
ESCAPE				:	'escape'		;
EVAL				:	'eval'			;		// evaluate
EVIDENT				:	'evident'		;
FINAL				:	'final'			;
FOR					:	'for'			;
FROM				:	'from'			;
GENERAL				:	'general'		;
IF					:	'if'			;
IMAGE				:	'image'			;
IN_					:	'in'			;
INCOMPLETE			:	'incomplete'	;
INIT				:	'init'			;		// initiate
INSTANCE			:	'instance'		;
INTERNAL			:	'internal'		;	
ISOLATE				:	'isolate'		;
LOOP				:	'loop'			;
METHOD				:	'method'		;
MISC				:	'misc'			;		// miscellaneous
NAF					:   'naf'			;		// not-a-function
NEW					:	'new'			;
NOM					:	'nom'			;		// nominal
NULL_				:	'null'			;
OPERATION			:	'operation'		;
OPT					:	'opt'			;		// optional
PACK				:	'pack'			;
PAGE				:	'page'			;
PROXY				:	'proxy'			;	
PWD					:	'pwd'			;		// previously well-defined
QUIT				:	'quit'			;
RETURN				:	'return'		;
SELECT				:	'select'		;	
STATIC				:	'static'		;
SUBROUTINE			:	'subroutine'	;
TBD					:	'tbd'			;		// to-be-defined 
TERM				:	'term'			;		// terminate
TRAP				:	'trap'			;
TYPE				:	'type'			;
USES				:   'uses'			;
UPD					:	'upd'			;		// update
VALUE				:	'value'			;
VOID_				:	'void'			;
WITH				:	'with'			;


// punctuation

LEFT_CURLY			:	'{'				;
RIGHT_CURLY			:	'}'				;
LEFT_PAREN			:	'('				;
RIGHT_PAREN			:	')'				;
LEFT_SQUARE			:	'['				;
RIGHT_SQUARE		:	']'				;	
COMMA				:	','				;
SEMI_COLON			:	';'				;
COLON				:	':'				;
EQUAL				:	'='				;
DOUBLE_QUOTE		:	'"'				;
ASTERISK			:	'*'				;
PLUS				:	'+'				;
MINUS				:	'-'				;
DIVIDE				:	'/'				;
ASSIGN				:	'->'			;


// literal 

				
fragment
LIT0				:	'\''						// delim
					;
	
fragment
LIT1				: [!-&(-\]_-~\r\n\t ]			// char subset: printable, except ^ and ', plus whitespace
					;

fragment
LIT2				: [0-9][0-9][0-9]				// 3 digits
					;

fragment			
LIT3				: '^' ( '^' | LIT0 | LIT2 )		// insert subexpr		
					;

LITERAL				: LIT0 ( LIT1 | LIT3 )*?  LIT0
					;


// operator


fragment
OP1					: [!-z{}~]						// char subset: printable, except SP and |
					;


OPERATOR			:  '|' OP1+? '|'
					| ( PLUS | MINUS | ASTERISK | DIVIDE )
					;


// source identifier


fragment
ID0					: ' '							// embedded space			
					;

fragment
ID1					: [a-zA-Z0-9_~@&!?]+			// name, char subset = alphanumeric plus _~@&!?
					;

fragment			
ID2					: '#' [a-zA-Z0-9]* 				// symbolic dimension 
					| LIT0 [0-9]+ LIT0				// adhoc dimension
					;

fragment												
ID3					: '<' ID0* ( ( ID2 | ID5 ) ID0* )? '>'	// factor   
					;

fragment											// with factors (type,method,subroutine)
ID4					: ID1 ( ID3+ ID1 )* ID3*	
					| ID3+ ( ID1 ID3+ )* ID1?
					;

fragment
ID5					: ( ID1 '\\' )? ID4				// qualified  
					;


SID					: ID5							// no sub-object
					| (( ID1 '\\' )? ID1 )? '.' ID1	// sub-object 
					;
					

// exclude 


REGION				: IGNORE_ .*? END		-> skip
					;

NARRATIVE			: '{{' .*? '}}'			-> skip
					;

REMARK				: '--' ~[\r\n]*			-> skip 
					;


// inter-token space (lowest precedence)

SPACE				: [ \t\r\n]+			-> skip
					;



// =====================  PARSER  ===========================
/*
	Good syntax is fairly simple and regular but context 
	determines usage, so context is captured low in the
	parse tree using redundant rules. 

	In the broad view, a Good procedure is comprised of
	executable expressions that create new objects and call
	other procedures.

	There are two kinds of procedures: method and subroutine.
	A method is defined within the scope of a type definition
	and references implied object "io".  A subroutine stands
	alone and can only reference an object's general methods.
	
	Any procedure can have inputs, outputs, and additional
	unspecified objects (extra). Subroutines can have auxiliary 
	inputs.	Variable inputs are labeled [upd].  

	Auxiliary inputs are used to pass context to a subroutine
	used as a coroutine. 
	
	A coroutine can be called by any method or subroutine
	to adapt it's algorithm.  Auxiliary inputs are given
	to the coroutine using a with-clause in the procedure 
	call.  A coroutine cannot itself call a coroutine 
	(adaptation is only one	level deep).

	A method is always called for some lead object given
	in the method call, separate from inputs and outputs.
	Method calls are annotated with a leading colon. The
	result of calling a method is (usually) the lead
	object.  Thus, methods can be chained together left-to-
	right, with	each method targeting an object to the 
	immediate left of the method, where the left object
	propagates from the previous method. The default 
	result can	be overridden by designating a particular
	output with an equal sign =.

	A subroutine call can generally be used wherever an 
	object is required provided the subroutine has a result.
	For	simplicity, grammar rules allow any (possibly non-
	conforming) subroutine call, but conformance will be 
	verified by the compiler.  

	
	Executable items can be grouped into executable blocks.
	
	Items and blocks can be incorporated into executable
	forms:  if/else, loop, for, select/value/else, 
	isolate/trap.

	A formula is an expression enclosed in double quotes.
	A formula has familiar syntax consisting of operations
	and traditional	function calls.  An operation is a
	sub-expression of operands and operators. A traditional
	function call has an implicit result given a list of 
	const inputs.
	
	Any procedure with const inputs and a single output/
	result can be called functionally inside a formula. A
	procedure that has side effects and/or a result that 
	depends on non-inputs is not suitable for formulae and 
	they can be excluded from consideration using [naf].
		
	Generally, definitions that introduce names can be
	truncated to allow references. These definitions are
	called "trivial."

	Here are possible object expressions and their
	semantics:
	
	SID				type ref:  new anon null obj, type = SID
	SID				obj ref:  existing obj, type = per def
	SID				proxy ref: existing obj, type = per proxy 
	SID				dimension: new anon initialized obj, type = expr
	SID				inconspicuous subroutine call: type = per proxy
	NULL_			new anon null input/output obj, type = per spec 
	LITERAL			new anon initialized obj, type = expr 
	new_obj			new named null obj, type and name specified
	new_analog		new anon initialized obj, type = analog 
	formula			new anon initialized obj, type = contextual 
	conversion		new anon initialized obj, type = specified
	method_call_sequence	existing result obj (method obj, result
					proxy, designated output)
	subroutine_call	existing result obj (result proxy, designated output)
*/


operator_				:  OPERATOR
						| ( PLUS | MINUS | ASTERISK | DIVIDE )
						;


// EXECUTABLE 



assignment_obj			: LITERAL
						| VOID_
						| SID
						| formula
						| method_call_sequence
						| subroutine_call
						| conversion
						;	

assignment				: ASSIGN assignment_obj
						;


proxy_assignment_ref	: SID	
						;

proxy_assignment		: proxy_assignment_ref assignment
						;


new_proxy_name			: SID
						;

new_proxy				: proxy_spec new_proxy_name assignment?
						;


new_obj_type_ref		: SID
						;

new_obj_name			: SID
						;

new_obj					: new_obj_type_ref new_obj_name	
						;


new_analog_ref			: SID	// proxy ref
						;

new_analog				: LEFT_SQUARE new_analog_ref RIGHT_SQUARE 
						;



conversion_obj			: LITERAL
						| SID
						| formula
						| method_call_sequence
						| subroutine_call
						;

conversion_type_ref		: SID
						;

conversion				: conversion_obj ( AS conversion_type_ref )+  // left to right
						;



formula_operand			: LITERAL
						| SID
						| formula_closure
						| formula_call
						| formula_operand ( AS SID )+  // special case conversion
						;

formula_term			: operator_? formula_operand
						;

formula_product			: formula_term ( operator_ formula_term )*   
						;

formula_closure			: LEFT_PAREN formula_product RIGHT_PAREN
						;

formula_input			: NULL_
						| new_analog
						| formula_product
						;

formula_ref				: SID  // proc ref
						;

formula_call			: formula_ref LEFT_PAREN ( formula_input ( COMMA formula_input )* )? RIGHT_PAREN
						;

formula					: DOUBLE_QUOTE formula_product DOUBLE_QUOTE 
						;



input_obj				: NULL_
						| LITERAL
						| SID
						| formula
						| method_call_sequence
						| subroutine_call	
						| conversion
						;

input_obj_item			: ASTERISK? input_obj
						;

input_obj_enum			: input_obj_item ( COMMA input_obj_item )*
						;

input_obj_list			: LEFT_PAREN input_obj_enum? RIGHT_PAREN		
						;


output_obj				: NULL_
						| SID
						| new_obj	
						| method_call_sequence	   
						| subroutine_call  
						;

output_obj_item			: EQUAL? output_obj
						;

output_obj_enum			: output_obj_item ( COMMA output_obj_item )*	// only one result
						;

output_obj_list			: LEFT_PAREN output_obj_enum? RIGHT_PAREN	
						;


extra_obj				: NULL_
						| LITERAL
						| SID
						| new_obj
						| formula
						| method_call_sequence
						| subroutine_call	
						| conversion
						;

extra_obj_enum			: extra_obj ( COMMA extra_obj )*
						;

extra_obj_list			: LEFT_PAREN extra_obj_enum? RIGHT_PAREN
						;




reg_given_obj_list		: input_obj_list
						| input_obj_list output_obj_list
						| input_obj_list output_obj_list extra_obj_list
						;

aux_given_obj_list		: LEFT_PAREN input_obj_list RIGHT_PAREN  
						;



call_coroutine_ref		: SID
						;

call_coroutine			: WITH call_coroutine_ref aux_given_obj_list?	
						;

call_proxy_attribution	: LEFT_SQUARE ( EVAL | UPD | INIT ) RIGHT_SQUARE
						;

call_proxy_name			: SID	
						;

call_provision			: reg_given_obj_list? call_coroutine? call_proxy_attribution? call_proxy_name?
						;


method_obj				: LITERAL
						| SID  
						| new_obj	
						| new_analog
						| formula
						| subroutine_call	
						;

method_ref				: SID
						;

method_call				: COLON method_ref call_provision
						;

method_call_sequence	: method_obj? method_call+
						;



subroutine_ref			: SID
						;

subroutine_call			: subroutine_ref call_provision
						;



quit_type_ref			: SID			
						;

quit_obj				: quit_type_ref  input_obj_list?
						;

quit					: QUIT quit_obj?	
						;



after					: AFTER ( subroutine_call | method_call_sequence | proxy_assignment )
						;


escape					: ESCAPE after?
						;


return					: RETURN after?
						;



condition_obj			: LITERAL	
						| SID
						| formula
						| method_call_sequence
						| subroutine_call
						;

condition				: LEFT_PAREN condition_obj RIGHT_PAREN	
						;


						
if						: IF condition exec_element
						| IF condition ( exec_element | exec_block ) ELSE exec_element
						;

if_block				: IF condition exec_block
						| IF condition ( exec_element | exec_block ) ELSE exec_block
						;


loop					: LOOP condition? exec_element	
						| LOOP condition		// pure loop
						;

loop_block				: LOOP condition? exec_block
						;


for_collection_obj		: SID	
						| method_call_sequence
						| subroutine_call
						;

for_index_obj			: SID	
						| new_obj	
						;

for_proxy_attribution	: LEFT_SQUARE ( EVAL | UPD | INIT ) RIGHT_SQUARE
						;

for_proxy_name			: SID  
						;

for_spec				: LEFT_PAREN for_proxy_attribution? for_proxy_name IN_ for_collection_obj ( COMMA for_index_obj )? RIGHT_PAREN
						;

for						: FOR for_spec exec_element
						;

for_block				: FOR for_spec exec_block
						;



isolate					: ISOLATE exec_element 
						| ISOLATE ( exec_element | exec_block ) TRAP exec_element 
						;

isolate_block			: ISOLATE exec_block
						| ISOLATE ( exec_element | exec_block ) TRAP exec_block
						;





select_value_obj		: LITERAL		 
						| SID		// pure const 
						;

select_value_list		: LEFT_PAREN select_value_obj ( COMMA select_value_obj )* RIGHT_PAREN
						;

select_branch_label		: VALUE select_value_list
						;

select_key_obj			: SID
						| formula
						| method_call_sequence
						| subroutine_call	
						;

select_labeled_branch	: select_branch_label exec_element? SEMI_COLON
						| select_branch_label ( COMMA? select_branch_label )* plain_block
						;

select_default_branch	: ELSE ( exec_block | exec_element SEMI_COLON )
						;

select_null_branch		: ELSE ( exec_block | exec_element SEMI_COLON )
						;

select_block			: SELECT LEFT_PAREN select_key_obj RIGHT_PAREN LEFT_CURLY select_labeled_branch* select_default_branch? RIGHT_CURLY select_null_branch?
						;



plain_block				: LEFT_CURLY exec_item* RIGHT_CURLY	
						;


exec_element			: new_obj	
						| new_proxy
						| proxy_assignment
						| method_call_sequence	
						| subroutine_call
						| if
						| loop
						| for
						| isolate
						| quit
						| escape
						| return
						;

exec_block				: plain_block			
						| if_block
						| loop_block
						| for_block
						| isolate_block
						| select_block
						;

exec_item				: exec_element? SEMI_COLON 
						| exec_block	
						;

non_exec_item			: subroutine
						| enum_type
						| nom_type
						| operation
						| common_obj	// syntactically non-executable
						;

proc_block				: LEFT_CURLY non_exec_item* exec_item* RIGHT_CURLY
						;




//---------------------------------------------------------------------  NON-EXECUTABLE



proxy_spec_attribute	: EVAL ( COMMA ( UPD | INIT ) )?
						| UPD
						| INIT
						;

proxy_spec_attribution	: LEFT_SQUARE proxy_spec_attribute RIGHT_SQUARE
						;

proxy_spec_type_ref		: SID
						;

proxy_spec				: proxy_spec_type_ref PROXY proxy_spec_attribution?   
						;



input_spec_type_ref		: SID
						;

input_spec_obj_name		: SID
						;

input_spec_attribute	: OPT
						| UPD
						;

input_spec_attribution	: LEFT_SQUARE input_spec_attribute RIGHT_SQUARE
						;

input_spec				: input_spec_type_ref input_spec_obj_name? input_spec_attribution?
						;

input_spec_enum			: input_spec ( COMMA input_spec )*
						;

input_spec_list			: LEFT_PAREN input_spec_enum? RIGHT_PAREN
						;


output_spec_type_ref	: SID
						;

output_spec_obj_name	: SID
						;

output_spec				: output_spec_type_ref output_spec_obj_name?
						;

output_spec_enum		: output_spec ( COMMA output_spec )*
						;

output_spec_list		: LEFT_PAREN output_spec_enum? RIGHT_PAREN
						;



extra_spec_type_ref		: SID  // alpha\extra
						;

extra_spec_obj_name		: SID
						;

extra_spec				: extra_spec_type_ref extra_spec_obj_name?
						;

extra_spec_list			: LEFT_PAREN extra_spec RIGHT_PAREN	
						;


reg_obj_spec_list		: input_spec_list 
						| input_spec_list output_spec_list
						| input_spec_list output_spec_list extra_spec_list
						;

aux_obj_spec_list		: LEFT_PAREN input_spec_list RIGHT_PAREN 
						;


coroutine_name			: SID
						;

coroutine_spec			: WITH coroutine_name reg_obj_spec_list?
						;


proxy_result_name		: SID
						;

proxy_result			: EQUAL proxy_spec proxy_result_name?
						;



subroutine_name			: SID
						;

subroutine_attribution	: LEFT_SQUARE NAF RIGHT_SQUARE
						;

subroutine_interface	: subroutine_attribution? aux_obj_spec_list? reg_obj_spec_list? coroutine_spec? proxy_result?
						;

subroutine_def			: subroutine_name subroutine_interface ( proc_block | SEMI_COLON )
						;

subroutine				: SUBROUTINE subroutine_def
						;

subroutine_group		: SUBROUTINE LEFT_CURLY subroutine_def* RIGHT_CURLY
						;



method_attribute		: ( EVAL | UPD | INIT | TERM ) ( COMMA NAF )?
						| NAF
						;

method_attribution		: LEFT_SQUARE method_attribute RIGHT_SQUARE
						;

method_interface		: method_attribution? reg_obj_spec_list? coroutine_spec? proxy_result? 
						;

method_name				: SID
						;

method_def				: method_name method_interface ( proc_block | SEMI_COLON )
						;

method					: ( GENERAL | ABSTRACT | BASE | MISC  ) METHOD method_def
						;

method_group			: ( GENERAL | ABSTRACT | BASE | MISC  ) METHOD? LEFT_CURLY method_def* RIGHT_CURLY
						;



recap_attribute			: TBD
						| ( PWD | NEW ) ( COMMA FINAL )?
						| FINAL
						;

recap_attribution		: LEFT_SQUARE recap_attribute RIGHT_SQUARE
						;

recap_base_ref			: SID
						;

recap_method_def		: recap_attribution? method_def
						;

recap_method			: ABSTRACT METHOD IN_ recap_base_ref recap_method_def
						;

recap_method_group		: ABSTRACT METHOD? IN_ recap_base_ref LEFT_CURLY recap_method_def* RIGHT_CURLY	
						;



instance_item_type_ref		: SID
							;

instance_item_obj_name		: SID
							;

instance_item_attribute		: OPT
							;

instance_item_attribution	: LEFT_SQUARE instance_item_attribute RIGHT_SQUARE
							;

instance_item				: instance_item_type_ref instance_item_obj_name? instance_item_attribution? SEMI_COLON 
							;

instance_block				: LEFT_CURLY instance_item* RIGHT_CURLY
							;

instance					: INSTANCE instance_block
							;


image_item_label		: LITERAL
						;

image_item_fex			: LITERAL
						;

image_item_input		: LEFT_PAREN image_item_fex? RIGHT_PAREN	
						;

image_item_extra		: LEFT_PAREN image_item_fex ( COMMA image_item_fex )* RIGHT_PAREN		
						;

image_item_obj_ref		: SID 
						;

image_item_include_key	: LITERAL
						;

image_item_include		: IMAGE image_item_include_key
						;

image_item				: image_item_obj_ref image_item_label? image_item_input image_item_extra? SEMI_COLON
						| image_item_include SEMI_COLON
						;

image_block				: LEFT_CURLY image_item* RIGHT_CURLY
						;

image_name				: SID
						;

image_key				: LITERAL
						;

image_attribute			: EVIDENT
						| COMPATIBLE ( COMMA ( ALIGN | PACK ) )?
						;
						
image_attribution		: LEFT_SQUARE image_attribute RIGHT_SQUARE 
						;

image_def				: image_name? image_key image_attribution? image_block
						;

image					: IMAGE image_def
						;

						

nom_type_name			: SID
						;

nom_type_ref			: SID
						;

nom_type_def			: nom_type_name ( EQUAL nom_type_ref )? SEMI_COLON
						;

nom_type				: NOM TYPE nom_type_def
						;

nom_type_group			: NOM TYPE LEFT_CURLY nom_type_def* RIGHT_CURLY
						;

						

enum_type_value			: LITERAL 
						;

enum_type_list			: LEFT_PAREN enum_type_value ( COMMA enum_type_value )* RIGHT_PAREN
						;

enum_type_name			: SID	
						;

enum_type_def			: enum_type_name enum_type_list? SEMI_COLON
						;

enum_type				: ENUM TYPE enum_type_def
						;

enum_type_group			: ENUM TYPE LEFT_CURLY enum_type_def* RIGHT_CURLY
						;



operation_ref			: SID
						;

operation_input			: NULL_
						| LITERAL
						| operation_operand	
						| operation_function
						;

operation_function		: operation_ref LEFT_PAREN operation_input ( COMMA operation_input )* RIGHT_PAREN	
						;

operation_operand		: SID		
						;

operation_unary_expr	: operator_ operation_operand
						;

operation_qnary_expr	: operation_operand ( operator_ operation_operand )+ 
						;

operation_expr			: operation_unary_expr
						| operation_qnary_expr
						;

operation_right_side	: EQUAL DOUBLE_QUOTE operation_function DOUBLE_QUOTE
						;

operation_left_side		: DOUBLE_QUOTE operation_expr DOUBLE_QUOTE
						;

operation_def			: operation_left_side operation_right_side? SEMI_COLON
						;
					
operation				: OPERATION operation_def
						;

operation_group			: OPERATION LEFT_CURLY operation_def* RIGHT_CURLY
						;



common_obj_type_ref		: SID
						;

common_obj_name			: SID
						;

common_obj_attribute	: CONST_
						;

common_obj_attribution	: LEFT_SQUARE common_obj_attribute RIGHT_SQUARE
						;

common_obj_proc			: SID proc_block   // SID = begin
						;

common_obj_subdef		: common_obj_name method_call? common_obj_attribution? SEMI_COLON  // method_call = :begin with literal inputs only
						;

common_obj_def			: common_obj_type_ref common_obj_subdef
						;

common_obj				: COMMON common_obj_def 
						;

common_obj_group		: COMMON LEFT_CURLY common_obj_def* common_obj_proc? RIGHT_CURLY
						| COMMON common_obj_type_ref LEFT_CURLY common_obj_subdef* common_obj_proc? RIGHT_CURLY
						;



type_item				: common_obj
						| common_obj_group
						| operation
						| operation_group
						| nom_type
						| nom_type_group
						| enum_type
						| enum_type_group
						| subroutine
						| subroutine_group
						| method
						| method_group
						| recap_method
						| recap_method_group
						| instance
						| image
						;

type_item_group			: LEFT_CURLY type_item* RIGHT_CURLY
						;

type_base_ref			: SID
						;

type_base_list			: LEFT_PAREN type_base_ref ( COMMA type_base_ref )* RIGHT_PAREN
						;

type_from				: FROM ( type_base_ref | type_base_list )
						;

type_from_sequence		: type_from ( COMMA? type_from )*
						;

type_attribute			: INCOMPLETE
						;

type_attribution		: LEFT_SQUARE type_attribute RIGHT_SQUARE
						;

type_name				: SID
						;

type_def				: type_name type_attribution? type_from_sequence? ( type_item_group | SEMI_COLON )
						;

type					: TYPE type_def
						;




page_internal			: LEFT_SQUARE INTERNAL RIGHT_SQUARE
						;

page_item				: page_internal? common_obj
						| page_internal? common_obj_group
						| page_internal? operation
						| page_internal? operation_group
						| page_internal? enum_type
						| page_internal? enum_type_group
						| page_internal? nom_type
						| page_internal? nom_type_group
						| page_internal? subroutine
						| page_internal? subroutine_group
						| page_internal? type
						;
						
page_uses_book_alias	: SID
						;

page_uses_book_ref		: SID
						;

page_uses_item			: page_uses_book_ref ( AS page_uses_book_alias )?
						;

page_uses_list			: LEFT_PAREN page_uses_item ( COMMA page_uses_item )* RIGHT_PAREN
						;

page_uses				: USES ( page_uses_item | page_uses_list )
						;

page_attribute			: COMPATIBLE
						;

page_attribution		: LEFT_SQUARE page_attribute RIGHT_SQUARE
						;

page_book_ref			: SID
						;

page_name				: SID
						;

page_header				: page_name IN_ page_book_ref page_attribution? page_uses*
						;

page					: PAGE page_header page_item* EOF
						;



