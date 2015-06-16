%{
/*
 * v2: 2014.12.28 增加数组和指针
 * v3: 2015.01.01 增加函数调用，能够翻译递归的斐波那契数列
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
//#include <unistd.h>
#include <string.h>

//#define NDEBUG	// assert的开关
#include <assert.h>
#define YYSTYPE struct SyntaxNode *
//#define SYN_DEBUG	// 语法树调试
//#define FREE_DEBUG	// 释放内存的调试
//#define TAC		// 三地址码模式

#define ARITH_EXP(operand,t0,t1,t2,t3)		\
	t2 = mknode(EXP);			\
	t2->u.op=operand;			\
	assert(t1->vartype == t3->vartype);	\
	t2->children[0]=t1;			\
	t2->children[1]=t3;			\
	t2->nchild=2;				\
	t2->vartype = t1->vartype;		\
	t0 = t2;				\
	syn_printline("arith_exp->...");	

#define REL_EXP(operand,t0,t1,t2,t3)		\
	t2 = mknode(EXP);			\
	t2->u.op=operand;			\
	assert(t1->vartype == t3->vartype);	\
	t2->children[0]=t1;			\
	t2->children[1]=t3;			\
	t2->nchild=2;				\
	t2->vartype = BOOLEAN;			\
	t0=t2;					\
	syn_printline("rel_exp->...");		\

#define LOGIC_EXP(operand,t0,t1,t2,t3)		\
	t2 = mknode(EXP);			\
	t2->u.op=operand;			\
	assert(t1->vartype == BOOLEAN);		\
	assert(t3->vartype == BOOLEAN);		\
	t2->children[0]=t1;			\
	t2->children[1]=t3;			\
	t2->nchild=2;				\
	t2->vartype = BOOLEAN;			\
	t0=t2;					

#define MAXCHILD 4	// 结点的最大孩子数
#define SHIFT 4		// 哈希函数的参数
#define HASHSIZE 20	// 哈希表的大小
#define NSTR 200	// 字符串表的大小
#define MAXSTR 20	// 变量的最大长度
#define MAXSIZE 256	// 临时缓冲区的大小(用于存储标号)
#define NPARA 3		// 函数参数的上限

extern FILE *yyin;

struct Symbol{
	int vtype;	// INT、 CHAR...
	int ftype;	// ID、ARRAY、FUNC...
	union{
		int size;	// 数组的大小
		struct {
			int para[NPARA];	// 函数的参数类型
			int npara;	// 函数的参数个数
			struct Env *env;	// 函数的作用域
		}f;
	}u;
	int name;
	int offset;		// 变量在栈中的偏移量
	struct Symbol *link;	// 哈希表指针
	int addrDesc[10];	// 地址描述符
	struct Triple *t;	// result指向该符号的三地址码
};

#define LIST_SIZE 200
struct List{
	int val;
	struct List *next;
}lists[LIST_SIZE];

struct SyntaxNode{
	int type;	// 结点类型
	int vartype;	// 变量类型
	union {
		int name;	// 标识符下标
		int op;		// 操作符
		int integer;
	}u;
	int nchild;
	int ref;	// ID类型的结点可能被引用多次
	int number;	// 结点编号
	struct Symbol *sym;
	int temp_var;
	struct SyntaxNode *children[MAXCHILD];
	struct SyntaxNode *sibling;
	union{
		struct{
			char *true_label;
			char *false_label;
		};
		struct{
			char *begin_label;
			char *next_label;
		};
	}label;
	union{
		struct{
			struct List *true_list;
			struct List *false_list;
		};
		struct{
			int begin_index;
			int next_index;
		};	
	}list;
	int printed;
};

struct Env{
	int curoff;
	struct Symbol *buckets[HASHSIZE];
	int temp_var_seq;
	struct Env *up;
	struct Env *next;
}st;
struct Env *env_start = &st;
struct Env *top;

#define TRIPLE_SIZE 200
struct Triple{
	int op;
	struct{
		union{
			int num;
			void *p;	// 开始指向四元组，之后指向符号表
		}u;
		int type;
	}arg[2];
	union{
		int obj;
		struct Symbol *result;
	};
	int last_ref;
	char *label;
	int index;
	int reg[2];
}triples[TRIPLE_SIZE];
int nins;
struct Triple *instructions[TRIPLE_SIZE];

enum Regs{
	EAX,EBX,ECX,EDX,
	ESI,EDI,ESP,EBP,
	NREG
};
char regs[NREG][4]={"eax","ebx","ecx","edx","esi","edi","esp","ebp"};
struct Symbol *regDesc[NREG][10];	// 对于寄存器i，[i][0]存放变量个数
					// ，[i][..]存放变量名
int usedReg[NREG];

#define TEMP_SIZE 20
int temp_index[TEMP_SIZE];

enum Nodetype{	
	PROGRAM, FUNC_LIST, FUNC,
	STMT_LIST,
	IF_STMT,WHILE_STMT,FOR_STMT,ASSIGN_STMT,DECL_STMT,CALL_STMT,RETURN_STMT,
	DECL_LIST,
	INPUT_STMT,OUTPUT_STMT,
	EXP,ARRAY,PARA,
	NON
};

enum Choice{		// struct SyntaxNode中的两种类型结点
	VTYPE=0,FTYPE
};

int lineno=1;
struct SyntaxNode *root=NULL;
int count=0;	// 结点编号
int curpara[3];	// 当前声明的函数的参数列表
int curnpara;
int curnum;	// 被词法分析程序赋值
int curid;	// 被词法分析程序赋值
int mkcount;	// 创建的结点数
int freecount;	// 删除的结点数
char storage[MAXSIZE];
char strtab[NSTR][MAXSTR];

struct SyntaxNode *mknode(int type);
void addtype(struct Symbol *, int ,int);
void free_mem();
void panic(char *);
char *getSymname(struct Symbol*);
void remove_node(struct SyntaxNode *);
void syn_printline(char *);
void printline(char *);
void print_buckets(struct Symbol **buckets);
void print_symbol(struct Symbol *);
void print_op(struct SyntaxNode *);
int getTemp();
struct Env *getEnv();
char *getName(struct SyntaxNode *);
struct Symbol *lookup_sym(char *, struct Symbol**);
struct Symbol *insert_sym(struct SyntaxNode *, struct Symbol**);

%}

%token LD ST JMP JL

%token MAIN READ PRINT RETURN
%token ID NUM INT CHAR BOOLEAN LINK
%token IF ELSE WHILE FOR
%token SEMI COM
%token LP RP LCB RCB LSB RSB
%token INC DEC
%token POINTER

%left ASSIGN

%left OR AND
%right NOT

%left EQ NE
%left GT GE LT LE

%left BOR
%left BXOR
%left BAND
%left LS RS
%left ADD SUB
%left MUL DIV MOD
%right BNOT 
%right UMINUS

%%

program: func_list main_func	{
		root=mknode(FUNC_LIST);
		root->nchild=2;
		root->children[0] = $1;
		root->children[1] = $2;
		printf("Have NT program\n");
		}
	| main_func	{root=$1;printf("Have NT program\n");}
	;

func_list: func_list func {
		$$=mknode(FUNC_LIST);
		$$->nchild=2;
		$$->children[0]=$1;
		$$->children[1]=$2;
		syn_printline("func_list->...");
		}
	| func	{$$=$1;syn_printline("func_list->func");}
	;

func: type id LP P2 para_list P3 RP block{
		remove_node($1);
		$2->sym = insert_sym($2,top->up->buckets);
		$2->sym->u.f.env = top;
		top = top->up;
		$2->nchild=1;
		$2->children[0] = $8;
		$$=$2;
		syn_printline("func->...");
	}
	;

P2: {
	struct Env *temp = getEnv();
	temp->up = top;
	top = temp;
	curnpara = 0;
	}
	;
P3:	{
	($-3)->type = FUNC;
	($-3)->vartype = ($-4)->type;
	insert_sym(($-3), top->buckets);
	}

para_list: para_list COM para
	| para
	|
	;

para: type id	{
		struct Symbol *temp;
		curpara[curnpara++] = $1->type;
		$2->vartype = $1->type;
		temp=insert_sym($2, top->buckets);
		switch($1->type){
		case INT:
			top->curoff += 4;
			temp->offset = top->curoff;
			break;
		case CHAR:
			top->curoff += 1;
			temp->offset = top->curoff;
			break;
		default:
			panic("para->type id: Unexpected type");
		}
		remove_node($1);
		remove_node($2);
		}
	;

main_func: MAIN LP RP block	{$$=$4;syn_printline("main_func->...");}
	;

block: LCB local_declarations stmt_list RCB		
	{ $$=mknode(STMT_LIST);
	$$->children[0]=$2;
	$$->children[1]=$3;
	$$->nchild=2;
	syn_printline("block->decl+stmt_list");}
	| LCB stmt_list RCB	{ $$=$2; syn_printline("block->stmt_list");}
	;
local_declarations: local_declarations declare_stmt SEMI
	{
	$$=mknode(DECL_LIST);
	$$->children[0]=$1;
	$$->children[1]=$2;
	$$->nchild=2;
	syn_printline("local_declarations->...");
	}
	| declare_stmt	SEMI{$$=$1;syn_printline("local_declarations->declare_stmt");}
	;

declare_stmt: type id_list {
		struct SyntaxNode *temp;
		$$ = mknode(DECL_STMT);
		$$->vartype = $1->type;
		remove_node($1);
		$$->children[0]=$2;
		$$->nchild=1;
		// 将声明插入符号表，并设置相应的值
		temp = $2;
		while(temp != NULL){
			temp->vartype = $$->vartype;
			temp->sym = insert_sym(temp,top->buckets);
			temp = temp->sibling;
		}
		syn_printline("declare_stmt->...");}
	;

type: INT {$$=mknode(INT);syn_printline("type->INT");}
	| CHAR {$$=mknode(CHAR);syn_printline("type->CHAR");}
	; 

id_list: id_list COM specifier {
			struct SyntaxNode *temp = $1;
			while(temp->sibling != NULL){
				temp = temp->sibling;
			}
			temp->sibling = $3;
			$$ = $1;
			} 
	| specifier	{
			$$ = $1;
			}
	;

specifier: id			{$$=$1;syn_printline("specifier->id");}
	| id LSB exp RSB	{
				$1->type = ARRAY;
				$1->nchild = 1;
				$1->children[0] = $3;
				$$ = $1;
				}
	| MUL id		{
				$2->type = POINTER;
				$$ = $2;
				}
	;

stmt_list: stmt_list stmt SEMI{
		$$=mknode(STMT_LIST);
		$$->children[0]=$1;
		$$->children[1]=$2;
		$$->nchild=2;
		syn_printline("stmt_list->...");
	}	
	| stmt SEMI{$$=$1;syn_printline("stmt_list->stmt");}
	;
 
stmt: if_stmt		{$$=$1;syn_printline("stmt->if_stmt");}
	| while_stmt	{$$=$1;}
	| for_stmt		{$$=$1;}
	| assign_stmt	{$$=$1;syn_printline("stmt->assign_stmt");}
	| input_stmt	{$$=$1;}
	| output_stmt	{$$=$1;}
	| call_stmt	{$$=$1;}
	| return_stmt {$$=$1;}
	;

return_stmt: RETURN id{
		$2->sym = lookup_sym(getName($2), top->buckets);
		assert($2->sym != NULL);
		$$ = mknode(RETURN_STMT);
		$$->nchild = 1;
		$$->children[0] = $2;
	}
		;

call_stmt: id LP id_list RP{
		struct SyntaxNode *temp;
		int i=0;
		$$ = mknode(CALL_STMT);
		$$->sym = lookup_sym(getName($1),top->buckets);
		assert($$->sym != NULL);
		remove_node($1);
		assert($$->sym->ftype == FUNC);
		temp=$3;
		while(temp != NULL){
			temp->sym = lookup_sym(getName(temp),top->buckets);
			assert(temp->sym != NULL);
			assert($$->sym->u.f.para[i] == temp->sym->vtype);
			i++;
			temp = temp->sibling;
		}
		assert(i==$$->sym->u.f.npara);
		$$->vartype = $$->sym->vtype;
		$$->nchild=1;
		$$->children[0]=$3;
	}
	| id LP RP{
		$$=mknode(CALL_STMT);
		$$->sym = lookup_sym(getName($1),top->buckets);
		assert($$->sym != NULL);
		assert($$->sym->ftype == FUNC);
		assert($$->sym->u.f.npara == 0);
		remove_node($1);
		}
	;

input_stmt: READ LP id RP 
		{
		$$=mknode(INPUT_STMT); 
		$$->children[0]=$3;
		$3->sym = lookup_sym(getName($3),top->buckets);
		assert($3->sym != NULL);
		$$->nchild=1;
		 }
	;

output_stmt: PRINT LP id RP
		{
		$$=mknode(OUTPUT_STMT); 
		$$->children[0]=$3;
		$3->sym = lookup_sym(getName($3),top->buckets);
		assert($3->sym != NULL);
		$$->nchild=1;
		}
	| PRINT LP num RP{
		// 注意————这一分支暂时只能用于调试
		$$=mknode(OUTPUT_STMT); $$->children[0]=$3;
		$$->nchild=1;
		}
	;
if_stmt: IF LP exp P1 RP block { $$=mknode(IF_STMT);$$->children[0]=$3;
		$$->children[1]=$6;$$->nchild=2;}	
	| IF LP exp P1 RP block ELSE block	{
		$$=mknode(IF_STMT);$$->children[0]=$3;
		$$->children[1]=$6;
		$$->children[2]=$8;
		$$->nchild=3;
	}
	;

P1:	 {assert($0->vartype==BOOLEAN); }
	;

while_stmt: WHILE LP exp P1 RP block {
	$$=mknode(WHILE_STMT);$$->children[0]=$3;
	$$->children[1]=$6;
	$$->nchild=2;
	}	
	;
	
for_stmt: FOR LP stmt SEMI exp P1 SEMI stmt RP block{
	$$=mknode(FOR_STMT);$$->children[0]=$3;
	$$->children[1]=$5;
	$$->children[2]=$8;
	$$->children[3]=$10;
	$$->nchild=4;
	}
	;
	
assign_stmt: specifier ASSIGN exp {
				$$=mknode(ASSIGN_STMT);
				$1->sym = lookup_sym(getName($1),top->buckets);
				assert($1->sym != NULL);
				assert($1->sym->vtype == $3->vartype);
				$$->children[0]=$1;
			    $$->children[1]=$3;
				$$->nchild=2;
				$$->u.op=ASSIGN;
				syn_printline("assign_stmt->...");
			}
	| special_assign_stmt {$$=$1;syn_printline("assign_stmt->special_assign_stmt");}
	;
	
special_assign_stmt: INC exp	{ 
			$$=mknode(ASSIGN_STMT);$$->u.op=INC;
			$$->children[0]=$2;
			$$->nchild=1;
			$$->temp_var = -2;	//区分前++和后++
			}
	| exp INC	{
			$$=mknode(ASSIGN_STMT);$$->u.op=INC;
			$$->children[0]=$1;
			$$->nchild=1;
			$$->temp_var = -3;
			}
	| DEC exp
			{
			$$=mknode(ASSIGN_STMT);$$->u.op=DEC;
			$$->children[0]=$2;
			$$->nchild=1;
			$$->temp_var = -2;
			}
	| exp DEC
			{
			$$=mknode(ASSIGN_STMT);$$->u.op=DEC;
			$$->children[0]=$1;
			$$->nchild=1;
			$$->temp_var = -2;
			}
	;

exp: exp AND exp	{ LOGIC_EXP(AND,$$,$1,$2,$3); }
	| exp OR exp	{ LOGIC_EXP(OR,$$,$1,$2,$3); }
	| rel_exp	{$$=$1;} 
	| NOT exp	{
		assert($2->vartype == BOOLEAN);
		$$=mknode(EXP);$$->u.op=NOT;
		$$->children[0]=$2;
		$$->nchild=1;
		$$->vartype = BOOLEAN;
	}
	;

rel_exp: rel_exp EQ rel_exp	{ REL_EXP(EQ,$$,$1,$2,$3); }
	| rel_exp GT rel_exp	{ REL_EXP(GT,$$,$1,$2,$3); }
	| rel_exp LT rel_exp	{ REL_EXP(LT,$$,$1,$2,$3); }
	| rel_exp GE rel_exp	{ REL_EXP(GE,$$,$1,$2,$3); }
	| rel_exp LE rel_exp	{ REL_EXP(LE,$$,$1,$2,$3); }
	| rel_exp NE rel_exp	{ REL_EXP(NE,$$,$1,$2,$3); }
	| arith_exp	{$$=$1;}
	;

arith_exp: arith_exp ADD arith_exp { ARITH_EXP(ADD,$$,$1,$2,$3); }
	| arith_exp SUB arith_exp { ARITH_EXP(SUB,$$,$1,$2,$3); }
	| arith_exp MUL arith_exp { ARITH_EXP(MUL,$$,$1,$2,$3); }
	| arith_exp DIV arith_exp { ARITH_EXP(DIV,$$,$1,$2,$3); }
	| arith_exp MOD arith_exp { ARITH_EXP(MOD,$$,$1,$2,$3); }
	| arith_exp BAND arith_exp { ARITH_EXP(BAND,$$,$1,$2,$3); }
	| arith_exp BOR arith_exp { ARITH_EXP(BOR,$$,$1,$2,$3); }
	| arith_exp BXOR arith_exp { ARITH_EXP(BXOR,$$,$1,$2,$3); }
	| arith_exp LS arith_exp { ARITH_EXP(LS,$$,$1,$2,$3); }
	| arith_exp RS arith_exp { ARITH_EXP(RS,$$,$1,$2,$3); }
	| num	{$1->vartype=INT;$$=$1;}
	| specifier {
		$1->sym = lookup_sym(getName($1), top->buckets);
		assert($1->sym != NULL);
		$1->vartype=$1->sym->vtype;$$=$1;
		}
	| call_stmt{
		$$ = $1;
		}
	| LP exp RP	{$$=$2;}
	| BNOT arith_exp{
		assert($2->vartype == INT);
		$$=mknode(EXP);$$->u.op=BNOT;
		$$->children[0]=$2;
		$$->nchild=1;
	}
	| SUB exp %prec UMINUS{
		assert($2->vartype == INT);
		$$=mknode(EXP);$$->u.op=UMINUS;
		$$->children[0]=$2;
		$$->nchild=1;
	}
	;


id: ID	{
	$$ = mknode(ID);
	$$->u.name = curid;
	syn_printline("id->ID");
	}
	;

num: NUM {
	$$ = mknode(NUM);
	$$->u.integer=curnum;
	}
	;

%%

void free_buckets(struct Symbol **);
void free_syntaxtree(struct SyntaxNode *);
void print_syntaxtree(struct SyntaxNode *);
void print_node(struct SyntaxNode *);
void print_type(int );
void print_tac();
void print_declare(struct SyntaxNode *);
void gen_code(FILE *, struct SyntaxNode*);
void gen_tac(struct SyntaxNode *);
void gen_header(FILE *);
void gen_decl(FILE *, struct SyntaxNode *);
void gen_ids(FILE *, struct SyntaxNode *);
void recursive_gen_decl(FILE *, struct SyntaxNode *);
void recursive_gen_code(FILE *, struct SyntaxNode *);
void recursive_func_gen_code(FILE *, struct SyntaxNode *);
void recursive_gen_tac(struct SyntaxNode *);
void expr_gen_tac(struct SyntaxNode *);
void stmt_gen_tac(struct SyntaxNode *);
void func_gen_code(FILE *, struct SyntaxNode *);
void expr_gen_code(FILE *,struct SyntaxNode *);
void stmt_gen_code(FILE *,struct SyntaxNode *);
void array_gen_code(FILE *, struct SyntaxNode *);
void rval_gen_code(FILE *, struct SyntaxNode *);
void lval_gen_code(FILE *, struct SyntaxNode *);
void recursive_get_label(struct SyntaxNode *);
void recursive_func_get_label(struct SyntaxNode *);
void func_get_label(struct SyntaxNode *);
void stmt_get_label(struct SyntaxNode *);
void expr_get_label(struct SyntaxNode *);
void get_label();
char *getNewlabel();
char *alloc(int);
int hash(char *);
int insert_str(char *str);
int insert_triple(int, struct SyntaxNode *, struct SyntaxNode *);
struct Triple *getTriple();
void triple_init();
void print_triple(struct Triple *);
void print_arg(struct Triple *,int);
void scan_tac();
int getTriptemp(int, int);
void tac_to_code(FILE *);
void itoa(int, char *);
void getReg(FILE *, struct Triple *);
int var_in_reg(struct Symbol *, int);
void loadVar(int, struct Symbol*);
int getEmptyReg();
void clearReg(FILE *, int);
void remove_reg(struct Symbol*, int);
void print_regDesc();
void clearSym(struct Symbol*);
void remove_sym(int, struct Symbol*);
void print_addrDesc(struct Symbol*);
int reg_no_use(int, struct Triple*);
struct List *mergeList(struct List*, struct List*);
void backpatch(struct List*, int);
struct List* makeList(int);
struct List* getList();
void loadArg(FILE *, struct Triple *, int);

int
main(void)
{
	FILE *fp = fopen("test.c","r");
	FILE *wp = fopen("out.asm","w");
	char *argv[]={"out.asm","opLabel.asm"};
	struct Env *temp;
	if(!fp){
		fprintf(stderr,"can not open file infile\n");
		return -1;
	}
	if(!wp){
		fprintf(stderr,"can not open file outfile\n");
		return -1;
	}
	yyin=fp;
	top = &st;
	yyparse(); 
	printf("main: Return from yyparse()\n");
	print_syntaxtree(root);
	printf("main: Return from print_syntaxtree()\n");
#ifdef TAC
	gen_tac(root);
	printf("main: Return from gen_tac()\n");
	scan_tac();
	printf("main: Return from scan_tac()\n");
	print_tac();
	tac_to_code(wp);
	printf("main: Return from tac_to_code()\n");
#else	
	#ifndef SYN_DEBUG
	get_label();
	printf("main():Maybe safely return from get_label()\n");
	#endif
	gen_code(wp,root);
#endif
	fclose(fp);
	fclose(wp);
	temp = env_start;
	while(temp!=NULL){
		print_buckets(temp->buckets);
		temp=temp->next;
	}
	free_mem();
	printf("mkcount=%d, freecount=%d\n",mkcount, freecount);

	return 0;
}

void
print_buckets(struct Symbol **buckets){
	int i=0;
	struct Symbol *temp=NULL;
	printf("----------Symbol Table----------\n");
	printf("name\ttype\tother\toffset\n");
	for(i=0;i<HASHSIZE;i++){
		if(buckets[i]!=NULL){
			temp = buckets[i];
			do{
				print_symbol(temp);
				temp = temp->link;
			}while(temp!=NULL);
		}
	}
	printf("--------------------------------\n");
}

void
print_symbol(struct Symbol *sym){
	int i;
	printf("%s\t",strtab[sym->name]);
	switch(sym->ftype){
	case ID:
		printf("ID\t");
		print_type(sym->vtype);
		break;
	case ARRAY:
		printf("ARRAY\t");
		print_type(sym->vtype);
		printf("\t%d",sym->u.size);
		break;
	case POINTER:
		printf("POINTER\t");
		print_type(sym->vtype);
		break;
	case FUNC:
		printf("FUNC\t");
		for(i=0;i<sym->u.f.npara;i++){
			print_type(sym->u.f.para[i]);
			printf(" ");
		}
		print_type(sym->vtype);
		printf("(r)");
		break;
	default:
		panic("print_symbol():unknown symbol type");
	}
	printf("\t%d", sym->offset);
	printf("\n");
}

void
free_buckets(struct Symbol **buckets){
	int i=0;
	struct Symbol *temp=NULL;
	for(i=0;i<HASHSIZE;i++){
		while(buckets[i]!=NULL){
			temp = buckets[i];
			buckets[i] = buckets[i]->link;
			free(temp);
			temp = NULL;
		}
	}
}

void
free_syntaxtree(struct SyntaxNode *node){
	int i=0;
	struct SyntaxNode *temp;
	if((node->type == DECL_STMT) || (node->type == CALL_STMT)){
		temp = node->children[0]->sibling;
		while(temp != NULL){
			node->children[0]->sibling = temp->sibling;
			free_syntaxtree(temp);
			temp = node->children[0]->sibling;
		}
	}
	if(node->type==ID || node->type == NUM){
		remove_node(node);
		return;
	}
	for(i=0;i<node->nchild;i++){
		free_syntaxtree(node->children[i]);
	}
	remove_node(node);
}

void
print_syntaxtree(struct SyntaxNode *node){
	int i=0;
	if(node==NULL)
		return;
	while(i<node->nchild){
		print_syntaxtree(node->children[i]);
		i++;
	}
	print_node(node);
}

struct SyntaxNode*
mknode(int type){
	struct SyntaxNode *node=NULL;
	int i=0;
	node = malloc(sizeof(*node));
	if(!node){
		panic("Out of memory!!\n");
	}
	node->type = type;
	node->u.integer = 0;
	node->nchild=0;
	node->ref=1;
	node->vartype=-1;
	node->number=count++;
	node->label.true_label=NULL;
	node->label.false_label=NULL;
	node->temp_var=-1;
	node->printed = 0;
	node->sibling = NULL;
	for(i=0;i<MAXCHILD;i++)
		node->children[i]=NULL;
	mkcount++;
	return node;
}

void
print_declare(struct SyntaxNode *node){
	struct SyntaxNode *temp = node->children[0]->sibling;
	printf("type: ");
	switch(node->vartype){
	case INT:
		printf("integer");
		break;
	case CHAR:
		printf("character");
		break;
	default:
		panic("print_declare():Unexpected type\n");
	}
	printf("\tid_list: %d", node->children[0]->number);
	while(temp != NULL){
		printf(" %d", temp->number);
		temp = temp->sibling;
	}
}

void
print_type(int type){
	switch(type){
	case INT:
		printf("INT");
		break;
	case CHAR:
		printf("CHAR");
		break;
	default:
		printf("type %d\n", type);
		panic("print_type(): Unexpected type");
	}
}

void
print_node(struct SyntaxNode *node){
	int i;
	struct SyntaxNode *temp;
	if(node==NULL){
		panic("print_node():NULL node");
	}
	if(node->printed == 1) return;
	printf("%d:",node->number);
	switch(node->type){
	case FUNC_LIST:
		printf("compound function\t");
		printf("\t");
		break;
	case FUNC:
		printf("function\t");
		printf("name: %s\t", getName(node));
		printf("para: ");
		for(i=0;i<node->sym->u.f.npara;i++){
			print_type(node->sym->u.f.para[i]);
			printf(" ");
		}
		print_type(node->sym->vtype);
		printf("(r)");
		printf("\t");
		break;
	case STMT_LIST:
		printf("compound statement\t");
		printf("\t");
		break;
	case DECL_LIST:
		printf("compound declarations\t");
		printf("\t");
		break;
	case IF_STMT:
		printf("if statement\t");
		printf("\t");
		break;
	case WHILE_STMT:
		printf("while statement\t");
		printf("\t");
		break;
	case FOR_STMT:
		printf("for statement\t");
		printf("\t");
		break;
	case ASSIGN_STMT:
		printf("assign statement\t");
		printf("op:");
		print_op(node);
		printf("\t");
		break;
	case DECL_STMT:
		printf("declare statement\t");
		print_declare(node);
		printf("\n");
		temp = node->children[0]->sibling;
		while(temp != NULL){
			print_syntaxtree(temp);
			temp = temp->sibling;
		}
		return;
	case CALL_STMT:
		printf("function call\t");
		printf("name: %s\t", getSymname(node->sym));
		if(node->nchild == 1){
			printf("id_list: ");
			temp = node->children[0];
			while(temp != NULL){
				printf(" %d", temp->number);
				temp = temp->sibling;
			}
			printf("\n");
			temp = node->children[0];
			while(temp != NULL){
				print_syntaxtree(temp);
				temp = temp->sibling;
			}
		}
		return;
	case EXP:
		printf("Exp\t");
		printf("op:");
		print_op(node);
		printf("\t");
		printf("temp_var=%d\t",node->temp_var);
		break;
	case ID:
		printf("ID Declaration\t");
		printf("symbol:%s\t",getName(node));
		break;
	case NUM:
		printf("Const Declaration\t");
		printf("Value:%d\t",node->u.integer);
		break;
	case ARRAY:
		printf("Array\t");
		printf("Name:%s Size:%d\t",getSymname(node->sym),node->sym->u.size);
		break;
	case POINTER:
		printf("Pointer\t");
		printf("Name:%s\t",getSymname(node->sym));
		break;
	case INPUT_STMT:
		printf("Input statement\t");
		printf("\t");
		break;
	case OUTPUT_STMT:
		printf("Output statement\t");
		printf("\t");
		break;
	case RETURN_STMT:
		printf("Return statement\t");
		printf("ID: %s\t", getName(node->children[0]));
		break;
	default:
		panic("unknown node_type");
		;
	}
	i=0;
	printf("children: ");
	while(i<node->nchild){
		printf("%-4d",node->children[i]->number);
		i++;
	}
	printf("\n");
	node->printed = 1;
}

void
addtype(struct Symbol *sym, int type, int choice){
	if(!sym){
		panic("addtype(): NULL pointer of Symbol");
	}
	switch(choice){
	case VTYPE:
		sym->vtype = type;
		break;
	case FTYPE:
		sym->ftype = type;
		break;
	default:
		panic("addtype(): Unexpected choice\n");
	}
}

void
syn_printline(char *str){
#ifdef SYN_DEBUG
	printline(str);
#endif
}

void
printline(char *str){
	printf("line %d:%s\n",lineno,str);
}

void
free_mem(){
	struct Env *temp = env_start->next;
	while(temp!=NULL){
		env_start->next = temp->next;
		free_buckets(temp->buckets);
		free(temp);
		temp = env_start->next;	
	}
	free_buckets(env_start->buckets);
#ifdef FREE_DEBUG
	printf("safely free buckets\n");
#endif
	free_syntaxtree(root);
#ifdef FREE_DEBUG
	printf("safely free syntaxtree\n");
#endif
}

void
panic(char *str){
	printline(str);
	free_mem();
	exit(1);
}

void
gen_code(FILE *wp, struct SyntaxNode  *root){
	struct SyntaxNode *main, *p;
	int decl_flag=0;
	if(root->type != FUNC_LIST){
		main = root;
	}else{
		main = root->children[1];
	}
	p = main->children[0];
	gen_header(wp);
	if(p->type == DECL_LIST || p->type == DECL_STMT){
		decl_flag = 1;
		gen_decl(wp,p);
	}
	fprintf(wp,"\n\n\t.code\n");
	// 先产生子程序代码
	if(root->type == FUNC_LIST){
		fprintf(wp,"\n");
		recursive_func_gen_code(wp, root->children[0]);
	}
	
	// 再产生MAIN函数代码
	fprintf(wp,"%s:\n",main->label.begin_label);
	fprintf(wp,"\tMOV ebp, esp\n");
	fprintf(wp,"\tSUB esp, %d\n",top->curoff+top->temp_var_seq*4);
#ifndef SYN_DEBUG
	if(decl_flag == 1)
		recursive_gen_code(wp, main->children[1]);
	else
		recursive_gen_code(wp, main);
#endif
	if(main->children[1]->label.next_label != NULL){
		fprintf(wp,"%s:\n",main->children[1]->label.next_label);
	}
	fprintf(wp,"\n");
	fprintf(wp,"\tinvoke StdIn, ADDR buffer, 127\n");
	fprintf(wp,"\tinvoke ExitProcess, 0\n");
	fprintf(wp,"end %s\n",main->label.begin_label);

}

void
recursive_func_gen_code(FILE *wp, struct SyntaxNode *node){
	if(node->type == FUNC_LIST){
		recursive_func_gen_code(wp,node->children[0]);
		func_gen_code(wp, node->children[1]);
		fprintf(wp,"\n");
	}else{
		func_gen_code(wp, node);
		fprintf(wp,"\n");
	}
}

void
func_gen_code(FILE *wp, struct SyntaxNode *node){
	int decl_flag = 0;
	int i;
	top = node->sym->u.f.env;
	fprintf(wp, "%s proc\n", getSymname(node->sym));
	if(node->children[0]->children[0]->type == DECL_STMT ||
	node->children[0]->children[0]->type == DECL_LIST){
		recursive_gen_decl(wp, node->children[0]->children[0]);
		decl_flag = 1;
	}
	i=1;
	while(i<=top->temp_var_seq){
		fprintf(wp, "\t\t_t%d DWORD 0\n", i);
		i++;
	}
	fprintf(wp, "\tPUSH ebp\n");
	fprintf(wp, "\tMOV ebp, esp\n");
	fprintf(wp, "\tSUB esp, %d\n", top->curoff + 4*top->temp_var_seq);
	for(i=0;i<node->sym->u.f.npara;i++){
		fprintf(wp, "\tMOV eax, [ebp+%d]\n", 8+4*(node->sym->u.f.npara-1-i));
		fprintf(wp, "\tMOV [ebp-%d], eax\n", 4+4*i);
	}
	if(decl_flag == 1)
		recursive_gen_code(wp,node->children[0]->children[1]);
	else
		recursive_gen_code(wp,node->children[0]);
	fprintf(wp, "%s endp\n", getSymname(node->sym));
	top = top->up;
}

void
gen_header(FILE *wp){
	fprintf(wp,"\t.586\n");
	fprintf(wp,"\t.model flat, stdcall\n");
	fprintf(wp,"\toption casemap :none\n");
	fprintf(wp,"\n");
	fprintf(wp,"\tinclude \\masm32\\include\\windows.inc\n");
	fprintf(wp,"\tinclude \\masm32\\include\\user32.inc\n");
	fprintf(wp,"\tinclude \\masm32\\include\\kernel32.inc\n");
	fprintf(wp,"\tinclude \\masm32\\include\\masm32.inc\n");
	fprintf(wp,"\n");
	fprintf(wp,"\tincludelib \\masm32\\lib\\user32.lib\n");
	fprintf(wp,"\tincludelib \\masm32\\lib\\kernel32.lib\n");
	fprintf(wp,"\tincludelib \\masm32\\lib\\masm32.lib\n");
}

char*
getSymname(struct Symbol *sym){
	return strtab[sym->name];
}

void
gen_decl(FILE *wp, struct SyntaxNode *node){
	int i=0;
	fprintf(wp,"\n\n\t.data\n");
	recursive_gen_decl(wp,node);
	for(i=1;i<=top->temp_var_seq;i++){
		fprintf(wp,"\t\tt%d%s\n",i," DWORD 0");
	}
	fprintf(wp,"\t\tbuffer BYTE 128 dup(0)\n");
	fprintf(wp,"\t\tLF BYTE 13, 10, 0\n");
}

void
stmt_gen_code(FILE *wp, struct SyntaxNode *node){
	struct SyntaxNode *e1=node->children[0];
	struct SyntaxNode *e2=node->children[1];
	struct SyntaxNode *e3=node->children[2];
	struct SyntaxNode *e4=node->children[3];
	struct SyntaxNode *temp;
	switch(node->type){
	case STMT_LIST:
		recursive_gen_code(wp,e1);
		fprintf(wp,"%s:\n",e1->label.next_label);
		recursive_gen_code(wp,e2);
		break;
	case WHILE_STMT:
		fprintf(wp,"%s:\n",e2->label.next_label);
		recursive_gen_code(wp,e1);
		fprintf(wp,"%s:\n",e1->label.true_label);
		recursive_gen_code(wp,e2);
		fprintf(wp,"\tjmp %s\n",e2->label.next_label);
		break;
	case INPUT_STMT:
		fprintf(wp,"\tinvoke StdIn, ADDR buffer, 127\n");
		fprintf(wp,"\tinvoke StripLF, ADDR buffer\n");
		fprintf(wp,"\tinvoke atodw, ADDR buffer\n");
		fprintf(wp,"\tMOV ");
		if(e1->type == ID)
			fprintf(wp,"[ebp-%d]",e1->sym->offset);
		fprintf(wp,", eax\n");
		break;
	case OUTPUT_STMT:
		fprintf(wp,"\tinvoke dwtoa, ");
		if(e1->type == ID)
			fprintf(wp,"[ebp-%d]",e1->sym->offset);
		fprintf(wp,", ADDR buffer\n");
		fprintf(wp,"\tinvoke StdOut, ADDR buffer\n");
		fprintf(wp,"\tinvoke StdOut, ADDR LF\n");
		break;
	case IF_STMT:
		if(node->nchild == 3){
			recursive_gen_code(wp,e1);
			fprintf(wp,"%s:\n",e1->label.true_label);
			recursive_gen_code(wp,e2);
			fprintf(wp,"\tjmp %s\n",node->label.next_label);
			fprintf(wp,"%s:\n",e1->label.false_label);
			recursive_gen_code(wp,e3);
		}else{
			assert(node->nchild == 2);
			recursive_gen_code(wp,e1);
			fprintf(wp,"%s:\n",e1->label.true_label);
			recursive_gen_code(wp,e2);
		}
		break;
	case FOR_STMT:
		recursive_gen_code(wp,e1);
		fprintf(wp,"%s:\n",e2->label.true_label);
		recursive_gen_code(wp,e4);
		recursive_gen_code(wp,e3);
		recursive_gen_code(wp,e2);
		break;
	case CALL_STMT:
		temp = node->children[0];
		fprintf(wp,"\tPUSHAD\n");
		while(temp!=NULL){
			fprintf(wp,"\tPUSH [ebp-%d]\n", temp->sym->offset);
			temp = temp->sibling;
		}
		
		fprintf(wp,"\tCALL %s\n", getSymname(node->sym));
		fprintf(wp,"\tMOV [ebp-%d], eax\n", top->curoff + 4*node->temp_var);
		fprintf(wp,"\tADD esp,%d\n", 4*node->sym->u.f.npara);
		fprintf(wp,"\tPOPAD\n");
		break;
	case RETURN_STMT:
		fprintf(wp,"\tMOV eax, [ebp-%d]\n", node->children[0]->sym->offset);
		fprintf(wp, "\tMOV esp, ebp\n");
		fprintf(wp, "\tPOP ebp\n");
		fprintf(wp, "\tRET\n");
		break;
	default:
		panic("stmt_gen_code():Unknown statement");
	}
}

void
lval_gen_code(FILE *wp, struct SyntaxNode *node){
	if(node->type == ID)
		fprintf(wp,"[ebp-%d]",node->sym->offset);
	else if((node->type == ARRAY) || (node->type == POINTER)){
		fprintf(wp,"[ebx]");
	}else{
		fprintf(wp,"[ebp-%d]",top->curoff+4*node->temp_var);
	}
}

void
rval_gen_code(FILE *wp, struct SyntaxNode *node){
	if(node->type == ID)
		fprintf(wp,"[ebp-%d]",node->sym->offset);
	else if(node->type == NUM)
		fprintf(wp,"%d",node->u.integer);
	else if((node->type == ARRAY) || (node->type == POINTER)){
		fprintf(wp,"[ebx]");
	}else{
		fprintf(wp,"[ebp-%d]",top->curoff+4*node->temp_var);
	}
}

void
expr_gen_code(FILE *wp, struct SyntaxNode *node){
	struct SyntaxNode *e1 = node->children[0];
	struct SyntaxNode *e2 = node->children[1];
	switch(node->u.op){
	case ASSIGN:
		recursive_gen_code(wp,e2);
		if(e2->type == ARRAY)
			array_gen_code(wp,e2);
		else if(e2->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e2->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp,e2);
		fprintf(wp,"\n");
		if(e1->type == ARRAY)
			array_gen_code(wp,e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV ");
		lval_gen_code(wp, e1);
		fprintf(wp,", eax\n");
		break;
	case INC:
	case DEC:
		recursive_gen_code(wp,e1);
		if(e1->type == ARRAY)
			array_gen_code(wp,e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp, e1);
		fprintf(wp,"\n");
		switch(node->u.op){
		case INC:
			fprintf(wp,"\tINC eax\n");
			break;
		case DEC:
			fprintf(wp,"\tDEC eax\n");
			break;
		};
		if(e1->type == ARRAY)
			array_gen_code(wp,e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV ");
		lval_gen_code(wp, e1);
		fprintf(wp,", eax\n");
		break;
	case ADD:
	case SUB:
	case BAND:
	case BOR:
	case BXOR:
	case LS:
	case RS:
	case MUL:
		recursive_gen_code(wp,e1);
		recursive_gen_code(wp,e2);
		if(e1->type == ARRAY)
			array_gen_code(wp, e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp, e1);
		fprintf(wp,"\n");
		if(e2->type == ARRAY)
			array_gen_code(wp, e2);
		else if(e2->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		switch(node->u.op){
		case ADD:
			fprintf(wp,"\tADD eax, ");
			break;
		case SUB:
			fprintf(wp,"\tSUB eax, ");
			break;
		case MUL:
			fprintf(wp,"\tMUL ");
			break;
		case BAND:
			fprintf(wp,"\tAND eax, ");
			break;
		case BOR:
			fprintf(wp,"\tOR eax, ");
			break;
		case BXOR:
			fprintf(wp,"\tXOR eax, ");
			break;
		case LS:
			fprintf(wp,"\tSAL eax, ");
			break;
		case RS:
			fprintf(wp,"\tSAR eax, ");
			break;
		}
		rval_gen_code(wp,e2);
		fprintf(wp,"\n");
		fprintf(wp,"\tMOV [ebp-%d], eax\n",top->curoff+4*node->temp_var);
		break;
	case BNOT:
	case UMINUS:
		recursive_gen_code(wp,e1);
		if(e1->type == ARRAY)
			array_gen_code(wp, e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp, e1);
		fprintf(wp,"\n");
		switch(node->u.op){
		case BNOT:
			fprintf(wp,"\tNOT eax\n");
			fprintf(wp,"\tMOV [ebp-%d], eax\n",top->curoff+4*node->temp_var);
			break;
		case UMINUS:
			fprintf(wp,"\tMOV ebx, 0\n");
			fprintf(wp,"\tSUB ebx, eax\n");
			fprintf(wp,"\tMOV [ebp-%d], ebx\n",top->curoff+4*node->temp_var);
			break;
		}
		break;
	case DIV:
	case MOD:
		recursive_gen_code(wp,e1);
		recursive_gen_code(wp,e2);
		if(e1->type == ARRAY)
			array_gen_code(wp, e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp, e1);
		fprintf(wp,"\n");
		if(e2->type == ARRAY)
			array_gen_code(wp, e2);
		else if(e2->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e2->sym->offset);
		fprintf(wp,"\tDIV ");
		rval_gen_code(wp, e2);
		fprintf(wp,"\n");
		switch(node->u.op){
		case DIV:
			fprintf(wp,"\tMOV [ebp-%d], eax\n",top->curoff+4*node->temp_var);
			break;
		case MOD:
			fprintf(wp,"\tMOV [ebp-%d], edx\n",top->curoff+4*node->temp_var);
			break;
		}
		break;
	case LT:
	case GT:
	case EQ:
	case NE:
	case GE:
	case LE:
		recursive_gen_code(wp,e1);
		recursive_gen_code(wp,e2);
		if(e1->type == ARRAY)
			array_gen_code(wp, e1);
		else if(e1->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e1->sym->offset);
		fprintf(wp,"\tMOV eax, ");
		rval_gen_code(wp, e1);
		fprintf(wp,"\n");
		if(e2->type == ARRAY)
			array_gen_code(wp, e2);
		else if(e2->type == POINTER)
			fprintf(wp,"\tMOV ebx, ADDR [ebp-%d]\n", e2->sym->offset);
		fprintf(wp,"\tCMP eax, ");
		rval_gen_code(wp, e2);
		fprintf(wp,"\n");
		switch(node->u.op){
		case LT:
			fprintf(wp,"\tjl %s\n",node->label.true_label);
			break;
		case GT:
			fprintf(wp,"\tjg %s\n",node->label.true_label);
			break;
		case EQ:
			fprintf(wp,"\tje %s\n",node->label.true_label);
			break;
		case NE:
			fprintf(wp,"\tjne %s\n",node->label.true_label);
			break;
		case GE:
			fprintf(wp,"\tjge %s\n",node->label.true_label);
			break;
		case LE:
			fprintf(wp,"\tjle %s\n",node->label.true_label);
			break;
		}
		fprintf(wp,"\tjmp %s\n",node->label.false_label);
		break;
	case AND:
		recursive_gen_code(wp,e1);
		fprintf(wp,"%s:\n",e1->label.true_label);
		recursive_gen_code(wp,e2);
		break;
	case OR:
		recursive_gen_code(wp,e1);
		fprintf(wp,"%s:\n",e1->label.false_label);
		recursive_gen_code(wp,e2);
		break;
	case NOT:
		recursive_gen_code(wp,e1);
		break;
	default:
		panic("expr_gen_code:Unknown node type");
	}
}

// 待访问的数组元素的地址放入EBX寄存器中
void
array_gen_code(FILE *wp, struct SyntaxNode *node){
	struct SyntaxNode *e;
	assert(node->type == ARRAY);
	e = node->children[0];
	fprintf(wp, "\tMOV edi, eax\n");
	fprintf(wp, "\tMOV ebx, ADDR [ebp-%d]\n", node->sym->offset);
	fprintf(wp, "\tMOV eax, ");
	if(e->type == ID)
		fprintf(wp,"[ebp-%d]",e->sym->offset);
	else if(e->type == NUM)
		fprintf(wp,"%d",e->u.integer);
	else
		fprintf(wp,"[ebp-%d]",top->curoff+4*(1+e->temp_var));
	fprintf(wp,"\n");
	if(node->sym->vtype == INT){
		fprintf(wp, "\tMUL 4\n");
	}else if(node->sym->vtype == CHAR){
		;
	}else{
		panic("array_gen_code(): Unexpected vtype");
	}	
	fprintf(wp, "\tADD ebx, eax\n");
	fprintf(wp, "\tMOV eax, edi\n");
}

void
recursive_gen_code(FILE *wp,struct SyntaxNode *node){
	switch(node->type){
	case STMT_LIST:
	case IF_STMT:
	case WHILE_STMT:
	case FOR_STMT:
	case INPUT_STMT:
	case OUTPUT_STMT:
	case CALL_STMT:
	case RETURN_STMT:
		stmt_gen_code(wp,node);
		break;
	case ASSIGN_STMT:
	case EXP:
		expr_gen_code(wp,node);
		break;
	case ID:
	case NUM:
	case ARRAY:
	case POINTER:
		break;
	default:
		printf("%d\n",ASSIGN_STMT);
		panic("recursive_gen_code(): Unexpected Node Type");
	}
}

void
recursive_gen_decl(FILE *wp, struct SyntaxNode *node){
	if(node->type == DECL_LIST){
		recursive_gen_decl(wp,node->children[0]);
		gen_ids(wp,node->children[1]);
		return;
	}else{
		gen_ids(wp,node);
	}
}

void
gen_ids(FILE *wp, struct SyntaxNode *node){
	struct SyntaxNode *temp;
	int vt,ft;
	assert(node->type == DECL_STMT);
	for(temp=node->children[0];temp!=NULL;temp=temp->sibling){
		vt = temp->sym->vtype;
		ft = temp->sym->ftype;
		switch(ft){
		case ID:
			switch(vt){
			case INT:
				fprintf(wp,"\t\t_%s DWORD 0\n",getName(temp));
				top->curoff+=4;
				temp->sym->offset = top->curoff;
				break;
			case CHAR:
				fprintf(wp,"\t\t_%s DWORD 0\n",getName(temp));
				top->curoff+=4;
				temp->sym->offset = top->curoff;
				break;
			default:
				panic("gen_ids: Unexpected vtype");
			}
			break;
		case ARRAY:
			switch(vt){
			case INT:
				fprintf(wp,"\t\t_%s DWORD %d dup (0)\n",getName(temp), temp->sym->u.size);
				top->curoff+=4*temp->sym->u.size;
				temp->sym->offset = top->curoff;
				break;
			case CHAR:
				fprintf(wp,"\t\t_%s DWORD %d dup (0)\n",getName(temp), temp->sym->u.size);
				top->curoff+=4*temp->sym->u.size;
				temp->sym->offset = top->curoff;
				break;
			default:
				panic("gen_ids: Unexpected vtype");
			}
			break;
		case POINTER:
			fprintf(wp,"\t\t_%s DWORD 0\n",getName(temp));
			top->curoff+=4;
			temp->sym->offset = top->curoff;
			break;
		default:
			panic("gen_ids: Unexpected ftype");
		}
	}
}

void
print_op(struct SyntaxNode *node){
	switch(node->u.op){
	case INC:
		printf("++");
		break;
	case DEC:
		printf("--");
		break;
	case ASSIGN:
		printf("=");
		break;
	case OR:
		printf("||");
		break;
	case AND:
		printf("&&");
		break;
	case BOR:
		printf("|");
		break;
	case BXOR:
		printf("^");
		break;
	case BAND:
		printf("&");
		break;
	case EQ:
		printf("==");
		break;
	case NE:
		printf("!=");
		break;
	case GT:
		printf(">");
		break;
	case GE:
		printf(">=");
		break;
	case LT:
		printf("<");
		break;
	case LE:
		printf("<=");
		break;
	case LS:
		printf("<<");
		break;
	case RS:
		printf(">>");
		break;
	case ADD:
		printf("+");
		break;
	case SUB:
		printf("-");
		break;
	case MUL:
		printf("*");
		break;
	case DIV:
		printf("%%");
		break;
	case BNOT:
		printf("~");
		break;
	case NOT:
		printf("!");
		break;
	default:
		panic("print_op():Unknown Op");
	}
}

void
get_label(){
	struct SyntaxNode *main, *p;
	if(root->type == FUNC_LIST){
		main = root->children[1];
	}else{
		main = root;
	}
	main->label.begin_label = "_start";
	main->children[1]->label.next_label=getNewlabel();
	printf("Before recrusive_get_label():\n");
	recursive_get_label(main->children[1]);
	printf("get_label():main function successfully get label\n");

	if(root->type == FUNC_LIST){
		p = root->children[0];
		recursive_func_get_label(p);
	}
}

void
recursive_func_get_label(struct SyntaxNode * node){
	if(node->type == FUNC_LIST){
		recursive_func_get_label(node->children[0]);
		func_get_label(node->children[1]);
	}else{
		func_get_label(node);
	}
}

void
func_get_label(struct SyntaxNode *node){
	node->children[0]->children[1]->label.next_label=getNewlabel();
	top = node->sym->u.f.env;
	recursive_get_label(node->children[0]->children[1]);
	top = top->up;
}

void
recursive_get_label(struct SyntaxNode *node){
	switch(node->type){
	case STMT_LIST:
	case IF_STMT:
	case WHILE_STMT:
	case FOR_STMT:
		stmt_get_label(node);
		break;
	case ASSIGN_STMT:
	case EXP:
	case CALL_STMT:
		expr_get_label(node);
		break;
	case INPUT_STMT:
	case OUTPUT_STMT:
	case NUM:
	case ID:
	case RETURN_STMT:
		break;
	default:
		panic("recursive_get_label():unknown node type");
	}
}

void
stmt_get_label(struct SyntaxNode *node){
	char *l1, *l2;
	struct SyntaxNode *e1 = node->children[0];
	struct SyntaxNode *e2 = node->children[1];
	struct SyntaxNode *e3 = node->children[2];
	struct SyntaxNode *e4 = node->children[3];
	switch(node->type){
	case STMT_LIST:
		l1 = getNewlabel();
		e1->label.next_label = l1;
		recursive_get_label(e1);
		e2->label.next_label = node->label.next_label;
		recursive_get_label(e2);
		break;
	case WHILE_STMT:
		l1 = getNewlabel();
		l2 = getNewlabel();
		e1->label.false_label = node->label.next_label;
		e1->label.true_label = l1;
		recursive_get_label(e1);
		e2->label.next_label = l2;
		recursive_get_label(e2);
		break;
	case IF_STMT:
		if(node->nchild == 3){
			l1 = getNewlabel();
			l2 = getNewlabel();
			e1->label.true_label = l1;
			e1->label.false_label = l2;
			recursive_get_label(e1);
			e2->label.next_label = node->label.next_label;
			recursive_get_label(e2);
			e3->label.next_label = node->label.next_label;
			recursive_get_label(e3);
		}else{
			assert(node->nchild == 2);
			l1 = getNewlabel();
			e1->label.true_label = l1;
			e1->label.false_label = node->label.next_label;
			recursive_get_label(e1);
			e2->label.next_label = node->label.next_label;
			recursive_get_label(e2);
		}
		break;
	case FOR_STMT:
		l1 = getNewlabel();
		e1->label.next_label = l1;
		recursive_get_label(e1);
		recursive_get_label(e4);
		recursive_get_label(e3);
		e2->label.true_label = l1;
		e2->label.false_label = node->label.next_label;
		recursive_get_label(e2);
		break;
	default:
		panic("stmt_get_label():unknown NODE type");
	}
}

void
expr_get_label(struct SyntaxNode *node){
	struct SyntaxNode *e1=node->children[0];
	struct SyntaxNode *e2=node->children[1];
	char *l1;
	switch(node->type){
	case EXP:
		node->temp_var = getTemp();
		if(e1 != NULL)
			recursive_get_label(e1);
		if(e2 != NULL)
			recursive_get_label(e2);
		break;	
	case ASSIGN_STMT:
		if(node->u.op == ASSIGN)
			recursive_get_label(e2);
		else
			recursive_get_label(e1);
		break;
	case CALL_STMT:
		node->temp_var = getTemp();
		break;
	default:
		panic("expr_get_label(): Unexpected node type");
	}
	switch(node->u.op){
	case AND:
		l1 = getNewlabel();
		e1->label.true_label = l1;
		e1->label.false_label = node->label.false_label;
		recursive_get_label(e1);
		e2->label.true_label = node->label.true_label;
		e2->label.false_label = node->label.false_label;
		recursive_get_label(e2);
		break;
	case OR:
		l1 = getNewlabel();
		e1->label.false_label = l1;
		e1->label.true_label = node->label.true_label;
		recursive_get_label(e1);
		e2->label.false_label = node->label.false_label;
		e2->label.true_label = node->label.true_label;
		recursive_get_label(e2);
		break;
	case NOT:
		e1->label.true_label = node->label.false_label;
		e1->label.false_label = node->label.true_label;
		break;
	default:
		break;
	}
}

char*
getNewlabel(){
	static int count = -1;
	char *str = alloc(3);
	sprintf(str,"@%d",++count);
	return str;
}

int
getTemp(){
	return ++(top->temp_var_seq);
}

char*
alloc(int size){
	static int next = 0;
	if((next+size) > MAXSIZE)
		panic("alloc():storage is running out.");
	next = next + size;
	return &storage[next-size];
}

int 
hash(char *key){
	int temp=0;
	int i=0;
	while(key[i]!=0){
		temp = ((temp <<SHIFT) + key[i]) % HASHSIZE;
		i++;
	}
	return temp;
}

struct Symbol *
lookup_sym(char *str, struct Symbol **buckets){
	int index = hash(str);
	struct Symbol *sym = buckets[index];
	while(sym != NULL){
		if(strcmp(strtab[sym->name],str) == 0){
			return sym;
		}
		sym = sym->link;
	}
	return NULL;
}

struct Symbol* 
insert_sym(struct SyntaxNode *node, struct Symbol **buckets){
	struct Symbol *sym;
	char *str = strtab[node->u.name];
	int index;
	int i;
	sym = lookup_sym(str, buckets);
	if(sym){
		return sym;
	}else{
		// 插入哈希表
		index=hash(str);
		if(buckets[index]){
			sym = (struct Symbol*)malloc(sizeof(*sym));
			memset(sym, 0, sizeof(*sym));
			if(!sym){
				printf("Out of memory!!\n");
				return NULL;
			}
			sym->link = buckets[index]->link;
			buckets[index]->link = sym;
		}else{
			sym = (struct Symbol *)malloc(sizeof(*sym));
			memset(sym, 0, sizeof(*sym));
			if(!sym){
				printf("Out of memory!!\n");
				return NULL;
			}
			buckets[index] = sym;
			sym->link = NULL;
		}
	}

	// 设置符号属性
	sym->name = node->u.name;
	switch(node->type){
	case ID:
		sym->vtype = node->vartype;
		sym->ftype = ID;
		break;
	case ARRAY:
		sym->vtype = node->vartype;
		sym->ftype = ARRAY;
		assert(node->children[0]->type == NUM);
		sym->u.size = node->children[0]->u.integer;
		break;
	case POINTER:
		sym->vtype = node->vartype;
		sym->ftype = POINTER;
		break;
	case FUNC:
		sym->ftype = FUNC;
		sym->vtype = node->vartype;
		// 函数的参数列表来源于全局变量
		for(i=0;i<curnpara;i++)
			sym->u.f.para[i] = curpara[i];
		sym->u.f.npara = curnpara;
		break;
	default:
		panic("insert_sym(): Unexpected type");
	}
	return sym;
	
}

int 
insert_str(char *str){
	static int curstr=0;
	int i=0;
	while(i<curstr){
		if(!strcmp(strtab[i],str)){
			break;
		}
		i++;
	}
	if(i==curstr){
		strncpy(strtab[curstr],str,MAXSTR);
		curstr++;
		if(curstr == NSTR) {
			printf("The strtab is full!!!\n");
			exit(1);
		}
	}
	return i;
}

void
remove_node(struct SyntaxNode *node){
	node->ref --;
	if(node->ref == 0){
#ifdef FREE_DEBUG
		printf("Ready to free node %d\n",node->number);
#endif
		free(node);
#ifdef FREE_DEBUG
		printf("After free node\n");
#endif
	}
	freecount++;
}

char*
getName(struct SyntaxNode *node){
	return strtab[node->u.name];
}

struct Env*
getEnv(){
	struct Env *e = malloc(sizeof(*e));
	struct Env *temp = env_start;
	if(e==NULL){
		printf("getEnv(): Out of memory!!\n");
		free_mem();
		exit(1);
	}
	while(temp->next != NULL)
		temp = temp->next;
	temp->next = e;
	memset(e,0,sizeof(*e));
	
	return e;
}

void
gen_tac(struct SyntaxNode *root){
	triple_init();
	recursive_gen_tac(root->children[1]);
}

void
print_tac(){
	int i;
	for(i=0;i<nins;i++){
		print_triple(instructions[i]);
	}
}

void
recursive_gen_tac(struct SyntaxNode *node){
	printf("recursive_gen_tac node %d\n",node->number);
	switch(node->type){
	case STMT_LIST:
	case IF_STMT:
		stmt_gen_tac(node);
		break;
	case ASSIGN_STMT:
	case EXP:
		expr_gen_tac(node);
		break;
	case ID:
	case NUM:
		break;
	default:
		printf("type=%d\n", node->type);
		panic("recursive_gen_tac: Unexpected node type");
	}
}

void
stmt_gen_tac(struct SyntaxNode *node){
	struct SyntaxNode *e1 = node->children[0];
	struct SyntaxNode *e2 = node->children[1];
	switch(node->type){
	case STMT_LIST:
		recursive_gen_tac(e1);
		recursive_gen_tac(e2);
		break;
	case IF_STMT:
		if(node->nchild == 2){
			recursive_gen_tac(e1);
			recursive_gen_tac(e2);
			node->list.next_index = e2->list.next_index;
			backpatch(e1->list.true_list, e2->list.begin_index);
			backpatch(e1->list.false_list, node->list.next_index);
		}else{
			panic("stmt_gen_tac: Not implemented");
		}
		break;
	default:
		printf("type=%d\n",node->type);
		panic("stmt_gen_tac: Unexpected node type");
	}
}

void
expr_gen_tac(struct SyntaxNode *node){
	struct SyntaxNode *e1 = node->children[0];
	struct SyntaxNode *e2 = node->children[1];
	int temp;
	switch(node->u.op){
	case ASSIGN:
		recursive_gen_tac(e2);
		node->list.begin_index=insert_triple(ASSIGN, e1, e2);
		node->list.next_index = node->list.begin_index+1;
		break;
	case ADD:
	case MUL:
		if(e1->type == EXP)
			recursive_gen_tac(e1);
		if(e2->type == EXP)
			recursive_gen_tac(e2);
		switch(node->u.op){
		case ADD:
			node->list.begin_index=insert_triple(ADD, e1, e2);
			break;
		case MUL:
			node->list.begin_index=insert_triple(MUL, e1, e2);
			break;
		}
		node->list.next_index = node->list.begin_index+1;
		break;
	case LT:
		node->list.begin_index=insert_triple(LT,e1,e2);
		node->list.true_list = makeList(insert_triple(JL,NULL,NULL));
		node->list.false_list = makeList(insert_triple(JMP,NULL,NULL));
		break;
	default:
		printf("Op = %d\n", node->u.op);
		panic("expr_gen_tac: Unexpected Op");
	}
}

int
insert_triple(int op, struct SyntaxNode *e1, struct SyntaxNode *e2){
	struct Triple *t;
	int i;
	struct SyntaxNode *temp = e1;
	instructions[nins++] = getTriple();
	t = instructions[nins-1];
	t->op = op;
	for(i=0;i<2;i++){
		if(temp == NULL){
			temp = e2;
			continue;
		}
		if(temp->type == ID){
			t->arg[i].u.p = temp->sym;
			t->arg[i].type = ID;
		}else if(temp->type == NUM){
			t->arg[i].u.num = temp->u.integer;
			t->arg[i].type = NUM;
		}else{
			t->arg[i].u.p = instructions[temp->list.begin_index];
			t->arg[i].type = LINK;
		}
		temp = e2;
	}
	t->index = nins - 1;
	return nins-1;
}

struct Triple*
getTriple(){
	static int last = 0;
	int i;
	for(i=last;i<TRIPLE_SIZE;i++){
		if(triples[i].op == NON){
			last = i;
			goto out;
		}
	}
	for(i=0;i<last;i++){
		if(triples[i].op == NON){
			last = i;
			goto out;
		}
	}
	panic("getTriple: Out of triple");
out:
	return &triples[i];
}

void
triple_init(){
	int i;
	for(i=0;i<TRIPLE_SIZE;i++)
		triples[i].op = NON;
}

void
print_triple(struct Triple *t){
	printf("%d: ",t->index);
	switch(t->op){
	case ASSIGN:
		printf("%s=", getSymname(t->arg[0].u.p));
		print_arg(t,1);
		break;
	case MUL:
	case ADD:
		printf("%s=",getSymname(t->result));
		print_arg(t,0);
		switch(t->op){
		case MUL:
			printf("*");
			break;
		case ADD:
			printf("+");
			break;
		}
		print_arg(t,1);
		break;
	case LT:
		print_arg(t,0);
		printf("<");
		 print_arg(t,1);
		break;
	case JL:
	case JMP:
		switch(t->op){
		case JL:
			printf("jl");
			break;
		case JMP:
			printf("jmp");
			break;
		}
		printf(" %d", t->obj);
		break;
	default:
		printf("t->op=%d\n",t->op);
		panic("print_triple: Unexpected op");
	}
	printf("\t%d\n",t->last_ref);
}

void
print_arg(struct Triple *t, int i){
	if(i != 0)
		assert(i == 1);
	switch(t->arg[i].type){
	case ID:
		printf("%s", getSymname(t->arg[i].u.p));
		break;
	case NUM:
		printf("%d", t->arg[i].u.num);
		break;
	case LINK:
		printf("%s", getSymname(t->arg[i].u.p));
		break;
	default:
		printf("type=%d\n",t->arg[i].type);
		panic("print_arg: Unexpected type");
	}
}

// 处理临时变量，标号
void
scan_tac(){
	int i,j;
	struct Triple *t;
	struct SyntaxNode *node;
	char str[4];
	int temp;

	node = mknode(ID);
	node->vartype = INT;
	str[0] = 't';
	for(i=nins-1;i>=0;i--){
		if((instructions[i]->op == JMP)
		|| (instructions[i]->op == JL)){
			t = instructions[instructions[i]->obj];
			if(t != NULL)
				t->label = getNewlabel();
			continue;
		}
		for(j=0;j<2;j++){
			if(instructions[i]->arg[j].type == LINK){
				t = instructions[i]->arg[j].u.p;
				if(t->last_ref == 0)
					t->last_ref = i;
				temp = getTriptemp(i, t->index);
				// 将临时变量加入符号表
				itoa(temp, &str[1]);
				node->u.name = insert_str(&str[0]);
				t->result = insert_sym(node, top->buckets);
				t->result->t = t;
				instructions[i]->arg[j].u.p = t->result;
			}
		}
	}
	remove_node(node);
}

int
getTriptemp(int curins, int index){
	int i=0;
	int next = top->temp_var_seq;
	while(i < next){
		if(curins <= temp_index[i]){
			temp_index[i] = index;
			return i; 
		}
		i++;
	}
	temp_index[i] = index;
	++(top->temp_var_seq);
	return i;
}

void
tac_to_code(FILE *wp){
	int i,j;
	struct Triple *t;
	struct Symbol *sym;
	printf("tac_to_code:\n");
	for(i=0;i<nins;i++){
		printf("%d:\n", i);
		t = instructions[i];
		if(t->label != NULL)
			fprintf(wp,"%s:\n", t->label);
		switch(t->op){
		case MUL:
		case ADD:
			getReg(wp, t);
			loadArg(wp,t,0);
			loadArg(wp,t,1);
			clearReg(wp, t->reg[0]);
			switch(t->op){
			case MUL:
				fprintf(wp,"MUL %s, %s\n", regs[t->reg[0]], regs[t->reg[1]]);
				break;
			case ADD:
				fprintf(wp,"ADD %s, %s\n", regs[t->reg[0]], regs[t->reg[1]]);
				break;
			}
			sym = t->arg[0].u.p;
			clearSym(t->result);
			loadVar(t->reg[0], t->result);
			break;
		case ASSIGN:
			getReg(wp, t);
			loadArg(wp,t,1);
			sym = t->arg[0].u.p;
			clearSym(sym);
			loadVar(t->reg[1], sym);
			break;
		case LT:
			getReg(wp, t);
			loadArg(wp,t,0);
			loadArg(wp,t,1);
			fprintf(wp,"CMP %s, %s\n", regs[t->reg[0]], regs[t->reg[1]]);
			break;
		case JMP:
			fprintf(wp,"JMP %s\n", instructions[t->obj]->label);
			break;
		case JL:
			fprintf(wp,"JL %s\n",instructions[t->obj]->label);
				break;	
		default:
			panic("tac_to_code: Unexpected type");
		}
		print_regDesc();
	}
	// 写回所有变量
	for(i=0;i<NREG;i++)
		clearReg(wp,i);
}

void
itoa(int num, char *str){
	int i=0;
	int j;
	char temp[10];
	do{
		str[i] = num % 10 + '0';
		num = num / 10;
		i++;
	}while(num != 0);

	for(j=0;j<i;j++)
		temp[j] = str[i-1-j];
	for(j=0;j<i;j++)
		str[j] = temp[j];
}

// getReg()的作用仅仅是分配合适的寄存器到t->reg[0]和t->reg[1]
// 只可能有ST操作
void
getReg(FILE *wp, struct Triple *t){
	// 三种基本情况
	// 1. 变量已在寄存器中
	// 2. 变量不在寄存器中，但有空寄存器
	// 3. 变量不在寄存器中，也没有空寄存器
	int i,j;
	int reg;
	struct Symbol *sym;
	int temp=-1;
	for(i=1;i>=0;i--){
		sym = t->arg[i].u.p;
		if((t->arg[i].type != NUM) && (sym->addrDesc[0] != 0)){
			sym = t->arg[i].u.p;
			reg = sym->addrDesc[1];
			t->reg[i] = reg;
			continue;
		}
		if((reg = getEmptyReg()) == -1){	// 情况3
			// 首先看有没有寄存器内的变量不再使用
			for(j=0;j<ESP;j++){
				if(reg_no_use(j,t) == 1){
					reg = j;
					break;
				}
			}
			if(reg == -1)	// 如果没找到...那就老办法
			{
				if(i == 0){
					if(t->reg[1] == ESI){
						reg = EDI;
					}else{
						reg = ESI;
					}
				}else{
					reg = ESI;
				}
			}
			clearReg(wp,reg);
		}
		if(i == 1){
			temp = reg;
			usedReg[temp]++;
		}
		t->reg[i] = reg;
		if(t->op == ASSIGN){
			t->reg[0] = reg;
			break;
		}
	}
	if(temp != -1){
		usedReg[temp]--;
	}
}

int
var_in_reg(struct Symbol *s, int reg){
	int end = usedReg[reg];
	int i=0;
	while(i<end){
		if(s == regDesc[reg][i])
			return 1;
		i++;
	}
	return 0;
}

void
loadVar(int reg, struct Symbol *s){
	int next = usedReg[reg];
	int last;
	if(next == 10)
		panic("loadVar: regDesc is full!!");
	// 修改寄存器描述符
	regDesc[reg][next] = s;
	usedReg[reg]++;
	// 修改地址描述符
	last = s->addrDesc[0];
	if(last == 9)
		panic("loadVar: addrDesc is full!!");
	s->addrDesc[last+1] = reg;
	s->addrDesc[0]++;
}

int
getEmptyReg(){
	int i;
	for(i=0;i<ESP;i++){	// ESP和EBP另有他用
		if(usedReg[i] == 0){
			return i;
		}
	}
	return -1;
}

void
clearReg(FILE *wp, int reg){
	int n = usedReg[reg];
	int i=0;
	struct Symbol *s;
	while(i<n){
		s = regDesc[reg][i];
		remove_reg(s, reg);
		fprintf(wp, "ST %s, %s\n", getSymname(s), regs[reg]);
		i++;
	}
	usedReg[reg] = 0;
}

// 从sym的地址描述符中移除reg
void
remove_reg(struct Symbol *s, int reg){
	int end = s->addrDesc[0] + 1;
	int i;
	for(i=1;i<end;i++){
		if(s->addrDesc[i] == reg)
			break;
	}
	if(i == end)
		panic("remove_reg: reg is not in the addDesc of symbol");
	while(i<(end-1)){
		s->addrDesc[i] = s->addrDesc[1+i];
		i++;
	}
	s->addrDesc[0]--;
}

// 从reg的寄存器描述符中删除变量s
void
remove_sym(int reg, struct Symbol *s){
	int n = usedReg[reg];
	int i=0;
	int j;
	while(i<n){
		if(regDesc[reg][i] == s)
			break;
		i++;
	}
	if(i==n)
		panic("remove_sym: Sym is not in the reg");
	j = i;
	while(j<(n-1)){
		regDesc[reg][j] = regDesc[reg][j+1];
		j++;
	}
	usedReg[reg]--;
}

void
print_regDesc(){
	int i,j;
	int n;
	for(i=0;i<NREG;i++){
		printf("%s:", regs[i]);
		n = usedReg[i];
		j=0;
		while(j<n){
			printf(" %s", getSymname(regDesc[i][j]));
			j++;
		}
		printf("\n");
	}
}

void
clearSym(struct Symbol *s){
	int n = s->addrDesc[0];
	int i=0;
	while(i<n){
		remove_sym(s->addrDesc[1+i], s);
		i++;
	}
	s->addrDesc[0] = 0;
}

void
print_addrDesc(struct Symbol *s){
	int n = s->addrDesc[0];
	int i=0;
	printf("%s:", getSymname(s));
	while(i<n){
		printf(" %s",regs[s->addrDesc[1+i]]);
		i++;
	}
	printf("\n");
}

int
reg_no_use(int reg, struct Triple *ct){
	int n = usedReg[reg];
	int i=0;
	struct Triple *t;
	while(i<n){
		t = regDesc[reg][i]->t;
		if(ct->index >= t->last_ref)
			return 1;
		i++;
	}
	return 0;
}

struct List*
getList(){
	static int next = 0;
	next++;
	if(next == LIST_SIZE)
		panic("getList: List if full!!");
	return &lists[next-1]; 
}

struct List*
makeList(int ins){
	struct List *l;
	l = getList();
	l->val = ins;
	l->next = NULL;
	return l;
}

struct List*
merge(struct List *l1, struct List *l2){
	struct List *l = l1;
	if(l == NULL)
		return l2;
	while(l->next != NULL){
		l = l->next;
	}
	l->next = l2;
	return l1;
}

void
backpatch(struct List *l, int ins){
	while(l != NULL){
		printf("backpatch: %d<<%d\n", l->val, ins);
		instructions[l->val]->obj = ins;
		l = l->next;
	}
}

void
loadArg(FILE *wp, struct Triple *t, int j){
	struct Symbol *sym;
	if(t->arg[j].type != NUM){
		sym = t->arg[j].u.p;
		if(var_in_reg(sym, t->reg[j]) == 0){
			loadVar(t->reg[j], sym);
			fprintf(wp,"LD %s, %s\n", regs[t->reg[j]], getSymname(sym));
		}
	}else{
		fprintf(wp,"LD %s, %d\n", regs[t->reg[j]], t->arg[j].u.num);
	}
}
