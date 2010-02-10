#include<stdio.h>
#include<string.h>
#include<sys/types.h>
#include<attr/xattr.h>
#include<attr/attributes.h>
#include<errno.h>


int print_init(){
    return printf("%c",131);
}

int print_atom(const char* atom){
    int len=strlen(atom);
    int i=0;
    printf("%c%c%c",100,0,len);
    for(;i<len;i++){
        printf("%c",atom[i]);
    }
    return len;
}

int print_tuple_header(int elems){
    return printf("%c%c",104,elems);
}

int print_string(const char* string){
    int len=strlen(string);
    if(len){
        int i=0;
        for(;i<len;i++){
            printf("%c",string[i]);
        }
        return len;
    } else {
        return printf("%c",106);
    }
}

void print_end(){
    printf(".");
    fflush(NULL);
}

void print_atom_tuple(const char* a, const char* b){
            print_init();
            print_tuple_header(2);
            print_atom(a);
            print_atom(b);
            print_end();
}

int list_file_print_error(const char* path, char* buffer, int length, int options, attrlist_cursor_t* cursor){
    if( attr_list(path,buffer,length,options,cursor) == -1){
        switch(errno){
            default: print_atom_tuple("error","list_error");
        }
        return -1;
    } else {
        return 0;
    }
}

int get_file_print_error(const char* path, const char* attr, char* val, int* length, int options){
    if( attr_get(path,attr,val,length,options) == -1 ){
        print_init();
        print_tuple_header(2);
        print_atom("error");
        switch(errno){
            case ENOATTR: 
                          print_atom("enoattr");/*The attribute in unreachable or does not exist.\n");*/ break;
            case ERANGE: 
                         print_atom("erange");/*The buffer size is too small\n");*/ break;
            case ENOTSUP: 
                          print_atom("enotsup");/*Extended arguments disabled or not supported on fs! (Or file missing)\n");*/ break;
            default: 
                     print_atom("unknown_get_error");
        }
        print_end();
        return -1;
    } else {
        return 0;
    }
}

int set_file_print_error(const char* path,const char* attr, const char *val,int length,int options){
    if (attr_set(path,attr,val,length,options) == -1){
        switch(errno){
            case E2BIG: print_atom_tuple("error","too_big"); break;
            default: print_atom_tuple("error","unknown_set_error"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

int remove_file_print_error(const char* path, const char* attr, int options){
    if(attr_remove(path,attr,options) == -1){
        switch(errno){
            case ENOATTR: print_atom_tuple("error","enoattr"); break;
            default: print_atom_tuple("error","some_error"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

//int calculate_binary_size(const char* string){
//   int g=0;
//   for(;string[i]&&string[i]!='.';i++)
//       switch(string[i]){
//           case '{':
//           case '[':
//           case '"':
//           case '\'':
//               g+=2; break;
//           default:
//               g+=i?string[i-1]==','?2:2:1; /*atoms either start a term or come after a  comma. */
//       }
//}
//
//int string_term_to_ext_binary(const char* string, char* binary, int length){
//   int i=0,
//       k=1;
//   char finished=0;
//   *binary=131; /* 
//                   100 0 # = atom
//                   104 0 ={}
//                   104 # = {...}
//                   106 = [] = ""
//                   107 0 # ... = "..."
//                   108 0 0 0 # ... 106 =[...]
//                   131=start
//                   */
//
//   if(calculate_binary_size(string)>=length){
//       *binary=0; /* what better way to report an error... ehm... */
//       return length;
//   }
//
//   for(i=0;string[i]&&string[i]!='.'&&!finished;){
//       int j;
//       switch(string[i]){
//           case '{': j=parse_curl(string[i],binary[k+2],&element_no);
//                     binary[i]=104;
//                     binary[i+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           case '[': j=parse_brac(string[i],binary[k+2],&element_no);
//                     binary[k]=108;
//                     binary[k+1]=j; /* this should be the number of elements, not the characters in total */
//                     i+=j;
//                     k+=j+2;
//                     break;
//           case '"': j=parse_string(string[i],binary[k+2],&element_no);
//                     binary[k]=108;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           default: j=parse_atom(string[i],binary[k+2]);
//                     binary[k]=100;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//       }
//   }
//   return i;
//}
//
//int parse(char sign, const char* string, char* binary){
//    int i=0,
//        j=0,
//        k=0;
//    for(;string[i]!=sign&&string[0];){
//       switch(string[i]){
//           case '\\': binary[i]
//           case '{': j=parse('}',string[i],binary[k+2]);
//                     binary[i]=104;
//                     binary[i+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           case '[': j=parse(']',string[i],binary[k+2]);
//                     binary[k]=108;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           case '"': j=parse('"',string[i],binary[k+2]); /*parse_string will in the future check for \" */
//                     binary[k]=108;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           case ''': j=parse(''',string[i],binary[k+2]); /* A strange atom */
//                     binary[k]=100;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//                     break;
//           default: j=parse_atom(string[i],binary[k+2]);
//                     binary[k]=100;
//                     binary[k+1]=j;
//                     i+=j;
//                     k+=j+2;
//       }
//    }
//}
//
//int parse_atom(const char* string, char* binary){
//    int i=0;
//    for(;string[0]&&string[i]!='['&&string[i]!='{'&&string[i]!=' '&&string[i]!=',';i++)
//        binary[i]=string[i];
//
//}
//
//
//
//
//

int main(){
    char path[1024],attr[256],val[256],command[1289]="attr -g "; /* Should be enough for anyone! */
    for(;;){
        scanf("%s",command);
        if(strcmp("e",command)){
            if(!strcmp("g",command)){
                int length=255;
                scanf("%s %s",path,attr);
                if(!get_file_print_error(path,attr,val,&length,0)){
                    val[length]=0;
                    print_init();
                    print_tuple_header(2);
                    print_atom("ok");
                    print_tuple_header(2);
                    print_string(attr);
                    print_string(val);
                    print_end();
                    //printf("{ok,{\"%s\",\"%s\"}}.\n",attr,val);
                }

            } else if(!strcmp("s",command)){
                scanf("%s %s %s",path,attr,val);
                if(!set_file_print_error(path,attr,val,strlen(val),0)){
                    printf("{ok,{\"%s\",\"%s\"}}.\n", attr, val);
                }

            } else if(!strcmp("t",command)){
                scanf("%s %s",path,attr);
                if(!set_file_print_error(path,attr,"",0,0)){
                    printf("{ok,{\"%s\",\"%s\"}}.\n", attr, "");
                }

            } else if(!strcmp("urt",command)){
                char bla[5]={131,100,0,1,65};
                int i=0;
                for(;i<5;i++)
                    printf("%c",bla[i]);
                printf(".\n");

                //                   100 0 # = atom
                //                   104 0 ={}
                //                   104 # = {...}
                //                   106 = [] = ""
                //                   107 0 # ... = "..."
                //                   108 0 0 0 # ... 106 =[...]
                //                   131=start
            } else if(!strcmp("r",command)){
                scanf("%s %s",path,attr);
                if(!remove_file_print_error(path,attr,0)){
                    //printf("ok.\n");
                    char bla[10]={131,100,0,2,'o','k'};
                    int i=0;
                    for(;i<6;i++)
                        printf("%c",bla[i]);
                    printf(".\n");
                }

            } else if(!strcmp("a",command)){
                scanf("%s %s %s",path,attr,val);
                char prev[256];
                int length=256;
                if( !get_file_print_error(path,attr,prev,&length,0) ) {
                    prev[length]=0;
                    strcat(prev,val);
                    if ( !set_file_print_error(path,attr,prev,strlen(prev),0)){
                        printf("{ok,{\"%s\",\"%s\"}}.\n", attr, prev);
                    }
                }

            } else if(!strcmp("l",command)){
                int length=2048;
                char buffer[length];
                attrlist_cursor_t cursor;
                scanf("%s",path);
                if (!list_file_print_error(path,buffer,length,0,&cursor)){
                    //attrlist_ent_t *ent_t;
                    __int32_t i,
                              count = ((attrlist_t*)buffer)->al_count;
                    printf("{ok,[");
                    for(i=count;i;i--) {
                        printf("\"%s\"",ATTR_ENTRY(buffer,i-1)->a_name);
                        printf(i-1?",":"");
                    }
                    printf("]}.\n");

                }

            } else if(!strcmp("L",command)){
                int length=2048;
                char buffer[length];
                attrlist_cursor_t cursor;
                scanf("%s",path);
                if( !list_file_print_error(path,buffer,length,0,&cursor)){
                    __int32_t i;
                    attrlist_t *list = (attrlist_t*) buffer;
                    __int32_t count = list->al_count;
                    //moar = list->al_more;
                    printf(count?"{ok,[":"{ok,[]}.\n");
                    for(i=count;i;i--){
                        attrlist_ent_t* ent = ATTR_ENTRY(buffer,i-1);
                        length=256; //ent->a_valuelen;
                        if( !get_file_print_error(path,ent->a_name,val,&length,0)) {
                            val[length]=0;
                            printf("{\"%s\",\"%s\"}",ent->a_name,val);
                            printf((i-1)?",":"]}.\n");
                        }
                    }

                }

            } else {
                print_atom_tuple("error","invalid_command");
                scanf("%*[^\n]");
            }
        } else {
            print_atom_tuple("exit","ok");
            return 0;
        }
        fflush(NULL);

    }

}
