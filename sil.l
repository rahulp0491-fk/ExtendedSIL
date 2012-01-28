%{
#include <stdlib.h>
#include <string.h>

#include "y.tab.h"

void yyerror(char *);

struct node {
	int value;
	int pos;
	char op;
	int relop;
	int type;
	char *s;
	int nodetype; //read=1, write=2, arithop=3, num=4, id=5, eqop=6   unused node = -1
	struct node *left, *right, *mid;
	struct symbol *lookup;
};
%}

%%



decl	{
	return DECL;
}
enddecl	{
	return ENDDECL;
}

begin	{
	return BEG;
}

end		{
	return END;
}

return	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->nodetype = 50;
	return RETURN;
}

if		{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left = NULL;
	yylval.nodeptr->right = NULL;
	yylval.nodeptr->nodetype = 8;
	return IF;
}

bool	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->nodetype = 22;
	return BOOL;
}

then	{
	return THEN;
}

endif	{
	return ENDIF;
}

endwhile	{
	return ENDWHILE;
}

else	{
	return ELSE;
}

typedef	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->nodetype = 60;
	return TYPEDEF;
}

while	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->nodetype = 9;
	return WHILE;
}

or 		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 17;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return OR;
}

and 		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 18;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return AND;
}

not		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 19;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return NOT;
}

do		{
	return DO;
}

read	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	//yylval.nodeptr->par=NULL;
	yylval.nodeptr->nodetype = 1;
	return READ;
}

write	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	//yylval.nodeptr->par=NULL;
	yylval.nodeptr->nodetype = 2;
	return WRITE;
}

false	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->type = 2;
	yylval.nodeptr->value = 0;
	yylval.nodeptr->nodetype = 30;
	return F;
}

true	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->value = 1;
	yylval.nodeptr->type = 2;
	yylval.nodeptr->nodetype = 31;
	return T;
}

main	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	//yylval.nodeptr->par=NULL;
	yylval.nodeptr->nodetype = 40;
	return MAIN;
}

int {
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	yylval.nodeptr->nodetype = 21;
	return INT;
}

[a-zA-Z][0-9a-zA-Z]*	{
	yylval.nodeptr = malloc (sizeof(struct node));
	yylval.nodeptr->s = malloc (yyleng);
	strcpy(yylval.nodeptr->s, yytext);
	yylval.nodeptr->nodetype = 5;
	yylval.nodeptr->left=NULL;
	yylval.nodeptr->right=NULL;
	//yylval.nodeptr->par=NULL;
	return ID;
}

[0-9]+	{
		yylval.nodeptr=malloc (sizeof(struct node));
		yylval.nodeptr->value=atoi(yytext);
		yylval.nodeptr->nodetype = 4;
		yylval.nodeptr->type = 1;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return NUM;
		}

[%]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='%';
		yylval.nodeptr->nodetype = 3;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return MOD;
}

[+]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='+';
		yylval.nodeptr->nodetype = 3;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return PLUSOP;
		}

[-]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='-';
		yylval.nodeptr->nodetype = 3;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return MINUSOP;
		}

[*]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='*';
		yylval.nodeptr->nodetype = 3;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return MULTIOP;
		}

[/]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='/';
		yylval.nodeptr->nodetype = 3;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return DIVIDEOP;
		}

[=]		{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->op='=';
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		//yylval.nodeptr->par=NULL;
		return EQOP;
		}

[>]	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 11;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return GT;
		}

[<]	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 12;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return LT;
		}

==	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 13;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return EQ;
		}

(<)(=) 	 {
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 15;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return LTEQ;
		}

>=	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 14;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return GTEQ;
		}



!=	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->relop = 16;
		yylval.nodeptr->nodetype = 6;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return NEQ;
		}

[,]	{
		yylval.nodeptr = malloc(sizeof(struct node));
		yylval.nodeptr->nodetype = 0;
		yylval.nodeptr->left=NULL;
		yylval.nodeptr->right=NULL;
		return COMA;
}

(\[) 	{
	return *yytext;
}

(\]) 	{
	return *yytext;
}

[.] {
	return *yytext;
}

[;(){}]	{
		return *yytext;
		}

[ \t\n] ;		/*do nothing for whitespace */

. yyerror("Invalid character");
%%

int yywrap(void)
{
return 1;
}


