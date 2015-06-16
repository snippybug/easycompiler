%{
#include <stdio.h>
#include <stdlib.h>
struct Label{
	int num;
	int ref;
	struct Label *next;
}head;
int insertLabel(int num,int ref);
struct Label *lookupLabel(int num);
int time;
FILE *outf;
%}


%%


^@[0-9]+:\n	{
		int num;
		int ret;
		num = atoi(yytext+1);
		if(time == 0){
			ret=insertLabel(num,0);
		}else{
			struct Label *L=lookupLabel(num);
			if(!L){
				printf("Head L:L should have a value\n");
				exit(1);
			}else{
				if(L->ref != 0){
					fprintf(outf,"%s",yytext);
				}
			}
		}
		}
@[0-9]+\n	{
			if(time == 0){
				int num;
				struct Label *L;
				num = atoi(yytext+1);
				L=lookupLabel(num);
				if(!L){
					insertLabel(num,1);
				}else{
					L->ref ++;
				}
			}else{
				fprintf(outf,"%s",yytext);
			}
		}

[^@]*	{
		if(time == 0){
			;
		}else{
			fprintf(outf,"%s",yytext);
		}
		}

%%

int insertLabel(int num,int ref){
	struct Label *L;
	L = lookupLabel(num);
	if(L)
		return 2;
	else{
			L=head.next;
			head.next = (struct Label*)malloc(sizeof(struct Label));
			head.next->num = num;
			head.next->ref = ref;
			head.next->next = L;
			return 1;
		}
}

struct Label *lookupLabel(int num){
	struct Label *temp=head.next;
	while(temp!=NULL){
		if(temp->num == num)
			break;
		temp=temp->next;
	}
	return temp;
}

void free_mem(){
	struct Label *L=head.next;
	while(L!=NULL){
		head.next = L->next;
		free(L);
		L=head.next;
	}
}

void print_label(){
	struct Label *L = head.next;
	while(L!=NULL){
		printf("L%d\n",L->num);
		L = L->next;
	}
}

int main(int argc, char **argv)
{
	FILE *inf1, *inf2;
	inf1 = fopen("out.asm","r");
	inf2 = fopen("out.asm","r");
	outf = fopen("op.asm","w");
	head.num = -1;
	head.ref = -1;
	head.next = NULL;
	
	time = 0;
	yyin = inf1;
	yylex();
	//print_label();
	time = 1;
	yyin = inf2;
	yylex();

	free_mem();	
	fclose(inf1);
	fclose(inf2);
	fclose(outf);
	return 0;
}

