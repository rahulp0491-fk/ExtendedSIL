%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int yylex(void);
void yyerror(char *);

struct symbol {
	char *name;
	int type;
	int size;
	int *binding;
	int isfun;
	struct typetree *ttype;
	struct arglist *link;
	struct symbol *next;
	
}*ghead = NULL, *lhead = NULL;

struct arglist {
	char *name;
	int type;
	int binding;
	struct arglist *next;
	struct typetree *tdef;
}*arghead, *hh;

struct node {
	int value;
	int pos;
	char op;
	int relop;
	int type;
	char *s;
	int nodetype;
	struct node *left, *right, *mid;
	struct symbol *lookup;
};

struct typetree {
	char *name;
	int size;
	struct arglist *i;
	struct arglist *b;
	struct arglist *n;
	struct typetree *next;
}*thead=NULL;

int t;
int v=1;
int returnmem = 4000;
int mem = 1000;
int reg = 0;
int label = 0;
struct symbol *ptr;
struct arglist *temp4;
int argcount;
int g = 1;
int f = 0;
int lbind = 1;
int abind = -2;
int flabel = 0;
int c = 0;
int argc=0;
int td = 0;
int relbind = 0;
int fcount = 0;
struct typetree *temp=NULL;
struct typetree *ndef = NULL;


void insert_sym (struct symbol *);
struct symbol * search_sym (char *, int);
struct arglist * search_arg (char *);
int evaluate(struct node *);
int calculate(struct node *);
void insert_tree (struct typetree *);
struct typetree * search_tree(char *);
%}

%union {
	struct node *nodeptr;
	int a;
}

%type <nodeptr> expr stmt  program slist id decl declist type boolvar flist argvarlist var fun dlist dec startfd args fcall function body argms typedefs starttd typedef tid ids
%token <nodeptr> EQOP PLUSOP MINUSOP MULTIOP DIVIDEOP NUM READ WRITE EQ GT LT IF WHILE LTEQ GTEQ NEQ AND OR NOT MOD MAIN RETURN ID INT T F COMA TYPEDEF BOOL
%token <a> DECL ENDDECL THEN ELSE DO ENDIF ENDWHILE BEG END
%left PLUSOP MINUSOP
%left MULTIOP DIVIDEOP

%%

program:	typedefs declaration body  {
		$$ = malloc(sizeof(struct node));
		$$->right = $3;
		$$->left = NULL;
	}
		| {}
		;

typedefs:	typedefs typedef {
		temp = NULL;
	}
		| {
		temp = NULL;
	}
		;

typedef:	starttd '{' declist '}' {
		struct typetree *s;
		s = search_tree($1->s);
		s->size = fcount;
		td = 0;
		fcount = 0;
	}
		;

starttd:	TYPEDEF ID {
			$$ = $2;
			relbind = 0;
			td = 1;
			struct typetree *temp1, *t, *s;
			temp = malloc (sizeof(struct typetree));
			temp1 = malloc (sizeof(struct typetree));
			temp1->name = malloc(sizeof($2->s));
			strcpy(temp1->name, $2->s);
			temp1->i = NULL;
			temp1->b = NULL;
			temp1->next = NULL;
			s = search_tree($2->s);
			if (s!=NULL)
				printf("This type already exists\n");
			else
				insert_tree(temp1);
			t = thead;
			while (t->next!=NULL) {
				t = t->next;
			}
			temp = t;
		}
			;

declaration:	DECL declist ENDDECL {
		temp = NULL;
		g = 0;
		//display(1);
		printf("START\n");
		printf("JMP M\n");
	}
		;

declist:	decl declist {temp = NULL;}
		| {}
		;

decl:	type dlist ';' {}
		;

dlist:	dlist COMA dec {}
	| dec {}
	;

dec:	var {}
	| flist {}
	;

type:	INT { 
		t = 1; 
	}
		| BOOL {
		t = 2;
	}
		| ID {
		struct typetree *s;
		s = search_tree($1->s);
		if (s!=NULL) {
			t = 3;
			if (td == 1) {
				ndef = s;
			}
			else 
				temp = s;
		}
		else {
			printf ("No userdefined type as such\n");
			t = -1;
		}
	}
		;

flist:	fun argdecl ')' {
		struct symbol *temp1, *ptr1;
		temp1 = malloc (sizeof (struct symbol));
		ptr1 = malloc (sizeof (struct symbol));
		ptr1 = search_sym($1->s, 1);
		temp1->name = malloc(sizeof($1->s));
		temp1->binding = malloc(sizeof(int));
		*temp1->binding = flabel++;
		temp1->isfun = 1;
		if (t == 1) 
			temp1->type = 1;
		if (t == 2)
			temp1->type = 2;
		temp1->size = argcount;
		strcpy(temp1->name, $1->s);
		temp1->next = NULL;
		if (g == 0)
			printf("Local function declaration prohibited\n");
		else {
			if (ptr1 !=NULL) {
				printf("Function already delcared\n");
			}
			else if (temp4 != NULL) 
				printf("Arguments repeated\n");
			else {
				insert_sym(temp1);
				ptr1 = search_sym ($1->s, 1);
				ptr1->link = arghead;
			}
		}
		v = 1;
	}
	;

fun:	ID '(' {
			$$ = $1;
			arghead = NULL;
			v = 2;
			argcount = 1;
		}
		;

argdecl : 	argvarlist { 
			v = 1;
			f = 0;
		}
		| {
		v = 1;
		f = 0;
		}
		;

argvarlist:	argvarlist COMA type var {}
		| type var {}
		;

var:	 ID {
		if (td == 1) {
			struct arglist *p, *s, *q, *l;
			s = malloc(sizeof(struct arglist));
			p = malloc(sizeof(struct arglist));
			p->name = malloc(sizeof($1->s));
			if (ndef == NULL)
				p->binding = relbind++;
			strcpy(p->name, $1->s);
			if (ndef != NULL) {
				p->tdef = ndef;
			}
			p->next = NULL;
			s = temp->i;
			while (s!=NULL) {
				if (!strcmp(s->name, $1->s)) 
					break;
				else
					s = s->next;
			}
			q = temp->b;
			while (q!=NULL) {
				if (!strcmp(q->name, $1->s)) 
					break;
				else
					q = q->next;
			}
			l = temp->n;
			while (l!=NULL) {
				if (!strcmp(l->name, $1->s)) 
					break;
				else
					l = l->next;
			}
			if (t == 1) {
				p->type = 1;
				if (temp->i == NULL) {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						temp->i = p;
						fcount++;
					}
				}
				else {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						s = temp->i;
						while (s->next!=NULL) 
								s = s->next;
						fcount++;
						s->next = p;
					
					}
				}
			}
			if (t == 2) {
				p->type = 2;
				if (temp->b == NULL) {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						temp->b = p;
						fcount++;
					}
				}
				else {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						s = temp->b;
						while (s->next!=NULL) {
								s = s->next;
						}
						fcount++;
						s->next = p;
					}
				}
			}
			if (t == 3) {
				p->type = 3;
				if (temp->n == NULL) {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						temp->n = p;
						fcount++;
					}
				}
				else {
					if(s!=NULL || q!=NULL || l!=NULL)
						printf("Var already exists\n");
					else {
						s = temp->n;
						while (s->next!=NULL) {
								s = s->next;
						}
						p->binding = (relbind-1) + p->tdef->size;
						relbind = relbind + p->tdef->size;
						fcount++;
						s->next = p;
					}
				}
			}
		}
		else {
				if (t == 1)
					$1->type = 1;
				if (t == 2)
					$1->type = 2;
				if (f == 1) {
					if (arghead != NULL) {
						if (!(arghead->type == $1->type)){
							f = 0;
							printf("Invalid argument\n");
						}
						arghead = arghead->next;
					}
				}
			if (v == 1 && f==0) {
					struct symbol *temp2, *temp3;
					temp2 = malloc (sizeof (struct symbol));
					temp3 = search_sym ($1->s, g);
					temp2->name = malloc(sizeof($1->s));
					temp2->binding = malloc(sizeof(int));
					if (g == 0)
						*temp2->binding = lbind++;
					if (g == 1){
						*temp2->binding = mem;
						if (t == 3)
							mem = mem + temp->size;
						else
							mem++;
					}
					if (t == 1)
						temp2->type = 1;
					if (t == 2)
						temp2->type = 2;
					if (t == 3) {
						temp2->type = 3;
						temp2->ttype = temp;
					}
					temp2->size = 1;
					strcpy(temp2->name, $1->s);
					temp2->next = NULL;
					if (temp3 !=NULL) 
						printf("Variable already delcared\n");
					else if (t == -1)
						printf("No such user defined type\n");
					else{
							insert_sym(temp2);
					}
				}
				if (v == 2) {
					struct arglist *temp2, *temp3;
					temp2 = malloc (sizeof (struct arglist));
					temp2->name = malloc(sizeof($1->s));
					temp2->next = NULL;
					strcpy(temp2->name, $1->s);
					if (t == 1)
						temp2->type = 1;
					if (t == 2)
						temp2->type = 2;
					if (arghead!=NULL)
						temp4 = search_arg($1->s);
					if (temp4!=NULL)
						printf("Var already declared\n");
					else {
						if (g == 0) {
							struct symbol *s;
							s = malloc (sizeof (struct symbol));
							s->name = malloc (sizeof($1->s));
							s->next = NULL;
							s->binding = malloc(sizeof(int));
							*s->binding = abind--;
							s->size = 1;
							strcpy(s->name, $1->s);
							if (t == 1)
								s->type = 1;
							if (t == 2)
								s->type = 2;
							insert_sym(s);
						}
						if (g == 1) {
							if (arghead== NULL) {
								arghead = temp2;
							}
							else {
								argcount++;
								temp3 = arghead;
								while (temp3->next != NULL) 
									temp3 = temp3->next;
								temp3->next = temp2;
							}
						}
					}
				}
		}
	}
		| ID '[' NUM ']' {
			struct symbol *temp2, *temp3;
			int i;
			temp2 = malloc (sizeof (struct symbol));
			temp3 = search_sym($1->s, 1);
			temp2->name = malloc(sizeof($1->s));
			if (t == 1) 
				temp2->type = 1;
			if (t == 2)
				temp2->type = 2;
			temp2->size = $3->value;
			temp2->binding = malloc($3->value*sizeof(int));
			for (i=0;i<$3->value;i++) 
				*(temp2->binding + i) = mem++;
			strcpy(temp2->name, $1->s);
			temp2->next = NULL;
			if (g == 0)
				printf("Local array declaration prohibited\n");
			else {
				if (temp3 !=NULL) {
					printf("Array already delcared");
				}
				else {
					insert_sym(temp2);
				}
			}
		};

body:	funlist main {
}
	;

funlist:	funlist function {}
	| {}
	;

function:	startfd argdecl ')' '{' DECL declist ENDDECL BEG slist END '}' {
			$$ = malloc(sizeof(struct node));
			$$->right = $9;
			printf("F%d:\n", *$1->lookup->binding);
			printf("PUSH BP\n");
			printf("MOV BP, SP\n");
			struct symbol *t;
			if (lhead !=NULL) {
				t = lhead;
				while (t!=NULL) {
					if (*t->binding > 0) {
						printf("PUSH R0\n");
					}
					t = t->next;
				}
			}
			evaluate($9);
			printf("MOV SP, BP\n");
			printf("POP BP\n");
			printf("RET\n");
			reg = 0;
		}
	;

startfd:	type ID '(' {
		f = 1;
		v = 2;
		lhead = NULL;
		lbind = 1;
		$2->lookup = search_sym ($2->s, 1);
		if ($2->lookup == NULL || $2->lookup->isfun != 1)
			printf("Function not declared\n");
		else {
			arghead = $2->lookup->link;
			$$ = $2;
		}
	}
	;

main:	 startmain '{' DECL declist ENDDECL BEG slist END '}' {
			printf("M:\n");
			printf("MOV SP 2000\n");
			printf("MOV BP, SP\n");
			struct symbol *t;
			if (lhead !=NULL) {
				t = lhead;
				while (t!=NULL) {
					if (*t->binding > 0) {
						printf("PUSH R0\n");
					}
					t = t->next;
				}
			}
			evaluate($7);
			printf("HALT\n");
			exit(0);
		}
	;

startmain:	INT MAIN '(' ')' {
			f = 0;
			v = 1;
			lhead = NULL;
			lbind = 1;
		}
	;

slist:	slist stmt  {
		$$ = malloc (sizeof(struct node));
		$$->nodetype = 0;
		$$->left = $1;
		$$->right = $2;
	}
		| {
		$$ = malloc(sizeof(struct node ));
		$$->nodetype = -1;
	}
	;

stmt:	READ '(' ids ')' ';' {
			$$ = malloc(sizeof(struct node));
			$1->right = $3;
			$$ = $1;
			}
		| WRITE '(' expr ')' ';' {
				$1->right = $3;
				$$ = $1;
			}

		| ids EQOP expr ';' {
				if ($1->nodetype == 60 && $1->left == NULL && $3->nodetype == 60 && $3->left == NULL) {
					$1->lookup = search_sym($1->s, 1);
					$3->lookup = search_sym($3->s, 1);
					if ($1->lookup->ttype == $3->lookup->ttype) {
						$$ = $2;
						$$->left = $1;
						$$->right = $3;
					}
					else
						yyerror("Type mismatch\n");
				}
				else {
					if ($1->type == $3->type ) {
						$$ = $2;
						$$->left = $1;
						$$->right = $3;
					}
					else
						yyerror("Type mismatch\n");
				}
		}
		| IF '(' expr ')' THEN '{' slist '}' ENDIF ';' {
			if ($3->type == 2 && $7->type == 0) {
				$1->right = $7;
				$1->left = $3;
				$$ = $1;
			}
			else 
				yyerror("Type mismatch\n");
		}
		| IF '(' expr ')' THEN '{' slist '}' ELSE '{' slist '}' ENDIF ';' {
			if ($3->type == 2 && $7->type == 0 && $11->type == 0) {
				$1->right = $7;
				$1->left = $3;
				$1->mid = $11;
				$$ = $1;
			}
			else 
				yyerror("Type mismatch\n");
		}
		| WHILE '(' expr ')' DO '{' slist '}' ENDWHILE ';' {
			$1->left = $3;
			$1->right = $7;
			$$ = $1;
		}
		| RETURN expr ';' {
			$1->right = $2;
			$$ = $1;
		}
		;

ids:	id { 
		$$ = $1; 
		}
	| tid {
	
	$$ = $1; 
	
	}
	;

idlist:	idlist id '.' {
		$$->left = $1;
		$$->
	}
	| {} 
;

tid:	tid ID {
			$2->lookup = search_sym($2->s, 1);
			if ($1->lookup == NULL)
				$1->lookup = search_sym($1->s, 0);
			struct arglist *s;
			if ($1->lookup->ttype!=NULL) {
				hh = $1->lookup->ttype->i;
				s = search_arg($3->s);
				if (s!=NULL) {
					$3->type = 1;
					$1->type = 1;
					$1->left = $3;
					$1->nodetype = 60;
					$$ = $1;
				}
				else {
					hh = $1->lookup->ttype->b;
					s = search_arg($3->s);
					if (s!=NULL) {
						$3->type = 2;
						$1->type = 2;
						$1->left = $3;
						$1->nodetype = 60;
						$$ = $1;
					}
					else {
						yyerror("No such field in this structure\n");
					}
				}
			}
		}
		| id '.' {
		
	}
	;

id:		ID {
	$1->lookup = search_sym($1->s, 0);
			if ($1->lookup == NULL) {
				$1->lookup = search_sym($1->s, 1);
				if ($1->lookup->type == 3) {
					$1->nodetype = 60;
				}
				if($1->lookup == NULL)
					yyerror("No global or local variable as such\n");
				else {
					$1->type = $1->lookup->type;
					$$ = $1;
				}
			}
			else {
				$1->type = $1->lookup->type;
				$$ = $1;
			}
	}
	| ID '[' expr ']' {
		$1->lookup = search_sym($1->s, 1);
		if ($1->lookup!=NULL) {
			$1->type = $1->lookup->type;
			$1->right = $3;
			$$ = $1;
		}
		else {
			yyerror("No such array declared\n");
		}
	}
	;

boolvar:	T {
	$$ = $1;
}
	| F 	 {
	$$ = $1;
	}
;

argms:	args {
	$$ = malloc(sizeof(struct node));
	$$ = $1;
}
	| {
	$$ = NULL;
	}
	;

args:	args COMA expr {
		$$ = malloc(sizeof(struct node));
		$2->left = $1;
		$2->right = $3;
		$$ = $2;
	}
	| expr { 
		$$ = malloc(sizeof(struct node));
		$$->right = $1; 
	}
	;

fcall:	ID '(' {
	c = 1;
	$1->nodetype = 40;
	$$ = $1;
}
;

expr:	NUM				{
						$$=$1;
						}
		| ids			{
						$$ = $1;
					}
		| fcall argms ')' {
						c = 0;
						$1->lookup = search_sym($1->s, 1);
						if ($1->lookup == NULL) {
							yyerror("Unknown function\n");
						}
						else {
							$1->type = $1->lookup->type;
							$$ = $1;
							$$->right = $2;
						}
					}
		| boolvar		{
						$$ = $1;
						}
		| expr PLUSOP expr	{
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 1;
							}
							else
								yyerror("Type Error\n");
						}
		| expr MINUSOP expr	{
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 1;
							}
							else
								yyerror("Type Error\n");
						}
		| expr MULTIOP expr	{
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 1;
							}
							else
								yyerror("Type Error\n");
						}
		| expr DIVIDEOP expr{
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 1;
							}
							else
								yyerror("Type Error\n");
						}
		| expr MOD expr		{
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 1;
							}
							else
								yyerror("Type Error\n");
					}
		|expr GT expr {
							if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
						}
		| expr LT expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		| expr LTEQ expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		| expr GTEQ expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		| expr EQ expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		| expr NEQ expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		|expr OR expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		|expr AND expr {
						if ($1->type == $3->type) {
								$$ = $2;
								$$->left = $1;
								$$->right = $3;
								$$->type = 2;
							}
							else
								yyerror("Type Error\n");
		}
		| NOT '(' expr ')'		{
						if ($3->type == 2) {
							$$=$1;
							$$->left=NULL;
							$$->right=$3;
							$$->type = 2;
							}
						else 
							yyerror("Type Error\n");
		}
		| '(' expr ')'	{
						$$=$2;
						}
		;
%%

void yyerror(char *s)
	{
	printf("%s\n", s);
	}

int tdisplay (){
	struct typetree *t;
	t = thead;
	while (t!=NULL) {
		t=t->next;
	}
}


void insert_tree (struct typetree *temp2) {
	struct typetree *t;
	if (thead == NULL){
		thead = temp2;
	}
	else {
		t = thead;
		while (t->next!=NULL) 
			t = t->next;
		t->next = temp2;
	}
}

void insert_sym (struct symbol *t) {
	struct symbol *s, *h;
	if (g == 0)
		h = lhead;
	if (g == 1)
		h = ghead;
	if (h == NULL) {
		if (g == 1)
			ghead = t;
		if (g == 0)
			lhead = t;
	}
	else {
		s = h;
		while(s->next != NULL)
			s = s->next;
		s->next = t;
	}
}

int display (int t) {
	struct symbol *s;
	if (t == 1)
		s = ghead;
	if (t == 0)
		s = lhead;
	while (s != NULL) {
		if (s->link != NULL && t == 1) {
			printf("%s\t", s->name);
			struct arglist *a;
			a = s->link;
			while (a!=NULL) {
				printf("%s\t", a->name);
				a = a->next;
			}
			printf("\n");
		}
		else
			printf("%s\n", s->name);
		s = s->next;
	}
}

struct symbol * search_sym (char *str, int s) {
	struct symbol *t;
	if (s == 1)
		t = ghead;
	if (s == 0)
		t = lhead;
	while (t!=NULL) {
		if (strcmp(str, t->name)== 0) 
			return t;
		else
			t = t->next;
		}
	return t;
}

struct arglist * search_arg ( char *str) {
	struct arglist *s;
	s = hh;
	while (s!=NULL) {
		if (strcmp(str, s->name)==0)
			return s;
		else
			s = s->next;
	}
	return s;
}

struct typetree * search_tree (char *str) {
	struct typetree *s;
	s = thead;
	while (s!=NULL) {
		if (strcmp(str, s->name)==0) 
			return s;
		else
			s = s->next;
	}
	return s;
}

int evaluate(struct node *e) {
	int l1, l2;
	int a,t,b, s;
	int i;
	struct arglist *p, *q;
	if (e != NULL && e->nodetype != -1){
		switch (e->nodetype) {
			case 0:
			evaluate(e->left);
			evaluate(e->right);
			break;
			
			case 1:
			t = 0;
			e->right->lookup = search_sym(e->right->s, 0);
			if (e->right->lookup==NULL) {
				t = 1;
				e->right->lookup = search_sym(e->right->s, 1);
			}
			if (e->right->lookup != NULL) {
				if (e->right->lookup->size == 1) {
					if (t != 1) {
						if(e->right->nodetype == 60) {
							a = reg;
							printf("IN R%d\n", a);
							printf("MOV R%d, BP\n", a+1);
							printf("MOV R%d, %d\n",a+2, *e->right->lookup->binding);
							printf("ADD R%d, R%d\n", a+1, a+2);
							struct arglist *p;
							if (e->right->type == 1) {
								hh = e->right->lookup->ttype->i;
								p = search_arg(e->right->left->s);
								printf("MOV R%d, %d\n",a+2, p->binding);
							}
							if (e->right->type == 2) {
								hh = e->right->lookup->ttype->b;
								p = search_arg(e->right->left->s);
								printf("MOV R%d, %d\n",a+2, p->binding);
							}
							printf("ADD R%d, R%d\n", a+1, a+2);
							printf("MOV [R%d], R%d\n", a+1, a);
							reg = a;
						}
						else {
							a = reg++;
							printf("IN R%d\n", a);
							printf("MOV R%d, BP\n", reg++);
							printf("MOV R%d, %d\n", reg, *e->right->lookup->binding);
							printf("ADD R%d, R%d\n", reg-1, reg);
							printf("MOV [R%d], R%d\n",reg-1, a);
							reg = a;
						}
					}
					else {
						a = reg++;
						printf("IN R%d\n", a);
						if(e->right->nodetype == 60) {
							printf("MOV R%d, %d\n",reg++, *e->right->lookup->binding);
							struct arglist *p;
							if (e->right->type == 1) {
								hh = e->right->lookup->ttype->i;
								p = search_arg(e->right->left->s);
								printf("MOV R%d, %d\n",reg, p->binding);
							}
							if (e->right->type == 2) {
								hh = e->right->lookup->ttype->b;
								p = search_arg(e->right->left->s);
								printf("MOV R%d, %d\n",reg, p->binding);
							}
							printf("ADD R%d, R%d\n", reg - 1, reg);
							printf("MOV [R%d], R%d\n", reg-1, a);
						}
						else
							printf("MOV [%d], R%d\n",*e->right->lookup->binding, a);
						reg = a;
					}
				}
				else {
					e->right->pos = calculate(e->right->right);
					a = reg++;
					printf("IN R%d\n", a);
					printf("MOV R%d, %d\n", reg, *e->right->lookup->binding);
					printf("ADD R%d, R%d\n", reg, e->right->pos);
					printf("MOV [R%d], R%d\n",reg , a);
					reg = e->right->pos;
				}
			}
			break;
			
			case 2:
			e->right->value = calculate(e->right);
			a = reg++;
			printf("MOV R%d, R%d\n",a, e->right->value);
			printf("OUT R%d\n", a);
			reg = e->right->value;
			break;
			
			case 6:
			t = 0;
			e->left->lookup = search_sym(e->left->s, 0);
			if (e->left->lookup == NULL) {
				t = 1;
				e->left->lookup = search_sym(e->left->s, 1);
			}
			if (e->left->lookup->type == 3)
				e->left->nodetype = 60;
			if(e->left->lookup->size != 1) {
				e->left->pos = calculate (e->left->right);
				printf("MOV R%d, %d\n",reg, *e->left->lookup->binding);
				printf("ADD R%d, R%d\n", e->left->pos, reg);
				printf("MOV [R%d], R%d\n", e->left->pos, calculate (e->right));
				reg = e->left->pos;
			}
			else {
				 if (e->left->nodetype == 60) {
					if (e->left->left == NULL) {
						q = e->right->lookup->ttype->b;
						p = e->right->lookup->ttype->i;
						s = 0;
						e->right->lookup = search_sym(e->right->s, 0);
						if (e->right->lookup == NULL) {
							s = 1;
							e->right->lookup = search_sym(e->right->s, 1);
						}
						if (t == 1) {
							a = reg;
							
							printf("MOV R%d, %d\n", a, *e->left->lookup->binding);
							if (s==1)
								printf("MOV R%d, %d\n", a+1, *e->right->lookup->binding);
							else {
								printf("MOV R%d, BP\n", a+1);
								printf("MOV R%d, %d\n", a+2, *e->right->lookup->binding);
								printf("ADD R%d, R%d\n", a+1, a+2);
							}
						}
						else {
							a = reg;
							printf("MOV R%d, BP\n", a);
							printf("MOV R%d, %d\n", a+1, *e->left->lookup->binding);
							printf("ADD R%d, R%d\n", a, a+1);
							if (s==1)
								printf("MOV R%d, %d\n", a+1, *e->right->lookup->binding);
							else {
								printf("MOV R%d, BP\n", a+1);
								printf("MOV R%d, %d\n", a+2, *e->right->lookup->binding);
								printf("ADD R%d, R%d\n", a+1, a+2);
							}
						}
						if (p!=NULL) {
							while (p!=NULL) {
								printf("MOV R%d, %d\n", a+2, p->binding);
								printf("ADD R%d, R%d\n", a+2, a+1);
								printf("MOV R%d, [R%d]\n", a+2, a+2);
								printf("MOV R%d, %d\n", a+3, p->binding);
								printf("ADD R%d, R%d\n", a+3, a);
								printf("MOV [R%d], R%d\n", a+3, a+2);
								p = p->next;
							}
						}
						if (q!=NULL) {
							while (q!=NULL) {
								printf("MOV R%d, %d\n", a+2, q->binding);
								printf("ADD R%d, R%d\n", a+2, a+1);
								printf("MOV R%d, [R%d]\n", a+2, a+2);
								printf("MOV R%d, %d\n", a+3, q->binding);
								printf("ADD R%d, R%d\n", a+3, a);
								printf("MOV [R%d], R%d\n", a+3, a+2);
								q = q->next;
							}
						}
						reg = a;
					}
					else {
						a = reg++;
						if (t == 1)
							printf("MOV R%d, %d\n",a, *e->left->lookup->binding);
						else {
							printf("MOV R%d, BP\n", a);
							printf("MOV R%d, %d\n", a+1, *e->left->lookup->binding);
							printf("ADD R%d, R%d\n", a, a+1);
						}
						if (e->left->type == 1) {
							hh = e->left->lookup->ttype->i;
							p = search_arg(e->left->left->s);
							printf("MOV R%d, %d\n",reg, p->binding);
						}
						if (e->left->type == 2) {
							hh = e->left->lookup->ttype->b;
							p = search_arg(e->left->left->s);
							printf("MOV R%d, %d\n",reg, p->binding);
						}
						printf("ADD R%d, R%d\n", a, reg);
						printf("MOV [R%d], R%d\n", a, calculate (e->right));
						reg = a;
					}
				}
				else {
				a = reg;
					if (t == 0) {
						printf("MOV R%d, BP\n", reg++);
						printf("MOV R%d, %d\n", reg, *e->left->lookup->binding);
						printf("ADD R%d, R%d\n", reg-1, reg);
						printf("MOV [R%d], R%d\n", a, calculate (e->right));
						reg = a;
					}
					else
						printf("MOV [%d], R%d\n", *e->left->lookup->binding, calculate (e->right));
				}
			}
			break;
			
			case 8:
			l1 = label;
			e->left->value = calculate(e->left);
			printf("JZ R%d, L%d\n", e->left->value, label++);
			reg = e->left->value;
			evaluate(e->right);
			if (e->mid!=NULL) {
				l2 = label;
				printf("JMP L%d\n", label++);
			}
			printf("L%d:\n", l1);
			if(e->mid!=NULL) {
				evaluate(e->mid);
				printf("L%d:\n", l2);
			}
			break;
			
			case 9:
			l2 = label;
			printf("L%d:\n", label++);
			e->left->value =  calculate(e->left);
			l1 = label;
			printf("JZ R%d, L%d\n", e->left->value, label++);
			reg = e->left->value;
			evaluate(e->right);
			printf("JMP L%d\n", l2);
			printf("L%d:\n", l1);
			break;
			
			case 40:
			calculate(e);
			break;
			
			case 50:
			printf("MOV [%d], R%d\n", returnmem, calculate(e->right));
			break;
			
			
			default:
			printf("Error\n");
			exit(1);
		}
	}
}


int calculate(struct node *e) {
				int x, y,i,a,t;
				switch(e->nodetype) {
					case 60:
					t = 0;
					e->lookup = search_sym(e->s, 0);
					if (e->lookup==NULL) {
						t = 1;
						e->lookup = search_sym(e->s, 1);
					}
					a = reg++;
					if (t == 0) {
						printf("MOV R%d, BP\n", a);
						printf("MOV R%d, %d\n", a+1, *e->lookup->binding);
						printf("ADD R%d, R%d\n", a, a+1);
					}
					else {
						printf("MOV R%d, %d\n",a, *e->lookup->binding);
					}
					if (e->lookup->size == 1) {
						struct arglist *p;
						if (e->type == 1) {
							hh = e->lookup->ttype->i;
							p = search_arg(e->left->s);
							printf("MOV R%d, %d\n",reg, p->binding);
						}
						if (e->type == 2) {
							hh = e->lookup->ttype->b;
							p = search_arg(e->left->s);
							printf("MOV R%d, %d\n",reg, p->binding);
						}
						printf("ADD R%d, R%d\n", a, reg);
						printf("MOV R%d, [R%d]\n", a, a);
						reg = a;
					}
					break;
					
					
					case 40:
					a = reg; 
					reg = 0;
					e->lookup = search_sym(e->s, 1);
					for (i=0;i<=a;i++) 
						printf("PUSH R%d\n", i);
					if (e->right != NULL) 
						calculate (e->right);
					printf("CALL F%d\n", *e->lookup->binding);
					if (e->right!=NULL) {	
						while (argc != 0) {
							printf("POP R0\n");
							argc--;
						}
					}
					for (i=a;i>=0;i--)
						printf("POP R%d\n", i);
					reg++;
					printf("MOV R%d, [%d]\n", reg, returnmem);
					break;
					
					case 0:
					x = calculate(e->right);
					printf("PUSH R%d\n", x);
					argc++;
					reg = x;
					if (e->left!=NULL) {
						y = calculate(e->left);
						if (e->left->nodetype != 0) {
							printf("PUSH R%d\n", y);
							argc++;
							reg = y;
						}
					}
					break;
					
					case 3:
					x = calculate(e->left);
					y = calculate(e->right);
					switch(e->op) {
						case '+':
						printf("ADD R%d, R%d\n", x, y);
						reg = x;
						break;
				
						case '-':
						printf("SUB R%d, R%d\n", x, y);
						reg = x;
						break;
				
						case '*':
						printf("MUL R%d, R%d\n", x, y);
						reg = x;
						break;
				
						case '/':
						printf("DIV R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case '%':
						printf("MOD R%d, R%d\n", x, y);
						reg = x;
						break;
						
						default:
						printf("Error in operators\n");
						exit(1);
					}
					break;
					
					case 5:
					t = 0;
					e->lookup = search_sym(e->s, 0);
					if (e->lookup==NULL) {
						t = 1;
						e->lookup = search_sym(e->s, 1);
					}
					if (e->lookup->size == 1) {
						if (t == 0) {
							e->value = reg;
							printf("MOV R%d, BP\n", reg++);
							printf("MOV R%d, %d\n", reg, *e->lookup->binding);
							printf("ADD R%d, R%d\n", reg-1, reg);
							printf("MOV R%d, [R%d]\n", e->value, reg-1);
							reg = e->value;
						}
						else {
							printf("MOV R%d, [%d]\n",reg, *e->lookup->binding);
						}
					}
					else {
						e->pos = calculate (e->right);
						printf("MOV R%d, %d\n",reg, *e->lookup->binding);
						printf("ADD R%d, R%d\n", e->pos, reg);
						reg = e->pos;
						printf("MOV R%d, [R%d]\n", reg, e->pos);
						
					}
					break;
					
					case 30:
					printf("MOV R%d, %d\n", reg, 0);
					break;
					
					case 31:
					printf("MOV R%d, %d\n", reg, 1);
					break;
					
					case 4:
					printf("MOV R%d, %d\n", reg, e->value);
					break;
					
					case 6:
					if (e->left!=NULL) 
						x = calculate(e->left);
					y = calculate(e->right);
					switch(e->relop) {
						case 11:
						printf("GT R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 12:
						printf("LT R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 13:
						printf("EQ R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 14:
						printf("GE R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 15:
						printf("LE R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 16:
						printf("NE R%d, R%d\n", x, y);
						reg = x;
						break;
						
						case 17:
						printf("ADD R%d, R%d\n", x, y);
						printf("MOV R%d, %d\n", reg, 1);
						printf("GE R%d, R%d\n", x, reg);
						reg = x;
						break;
						
						case 18:
						printf("ADD R%d, R%d\n", x, y);
						printf("MOV R%d, %d\n", reg, 1);
						printf("GT R%d R%d\n", x, reg);
						reg = x;
						break;
						
						/*case 19:
						printf("GT ");
						break;*/
					}
					break;
					
					default:
					printf("Error\n");
					exit(1);
			}
			return reg++;
}

int main(void) {
	yyparse();
	return 0;
}

