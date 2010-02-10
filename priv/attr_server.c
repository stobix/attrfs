#include<stdio.h>
#include<string.h>
#include<sys/types.h>
#include<attr/xattr.h>
#include<attr/attributes.h>
#include<errno.h>

/**********************/

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
        printf("%c%c%c",107,0,len);
        for(;i<len;i++){
            printf("%c",string[i]);
        }
        return len;
    } else {
        return printf("%c",106);
    }
}

void print_list_header(char elems){
    printf("%c%c%c%c%c",108,0,0,0,elems);
}

void print_list_end(){
    printf("%c",106);
}

void print_empty_list(){
    printf("%c",106);
}

void print_end(){
    printf(".");
    fflush(NULL);
}

void print_atom_tuple_response(const char* a, const char* b){
            print_init();
            print_tuple_header(2);
            print_atom(a);
            print_atom(b);
            print_end();
}

void print_string_tuple(const char* a, const char* b){
            print_tuple_header(2);
            print_string(a);
            print_string(b);
}


void print_error(const char* err){ /* change this to print_error_response or something with a more consistent nomenclature */
    print_atom_tuple_response("error",err);
}

void print_ok_string_tuple(const char* str, const char * Str){ /* also this function needs a better name */
    print_init();
    print_tuple_header(2);
    print_atom("ok");
    print_tuple_header(2);
    print_string(str);
    print_string(Str);
    print_end();
}


/**********************/

int list_file_print_error(const char* path, char* buffer, int length, int options, attrlist_cursor_t* cursor){
    if( attr_list(path,buffer,length,options,cursor) == -1){
        switch(errno){
            default: print_error("list_error");
        }
        return -1;
    } else {
        return 0;
    }
}

int get_file_print_error(const char* path, const char* attr, char* val, int* length, int options){
    if( attr_get(path,attr,val,length,options) == -1 ){
        switch(errno){
            case ENOATTR: 
                print_error("enoattr");/*The attribute in unreachable or does not exist.");*/ break;
            case ERANGE: 
                print_error("erange");/*The buffer size is too small");*/ break;
            case ENOTSUP: 
                print_error("enotsup");/*Extended arguments disabled or not supported on fs! (Or file missing)");*/ break;
            default: 
                print_error("unknown_get_error");
        }
        return -1;
    } else {
        return 0;
    }
}

int set_file_print_error(const char* path,const char* attr, const char *val,int length,int options){
    if (attr_set(path,attr,val,length,options) == -1){
        switch(errno){
            case E2BIG: print_error("too_big"); break;
            default: print_error("unknown_set_error"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

int remove_file_print_error(const char* path, const char* attr, int options){
    if(attr_remove(path,attr,options) == -1){
        switch(errno){
            case ENOATTR: print_error("enoattr"); break;
            default: print_error("unparsed_remove_error"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

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
                    print_ok_string_tuple(attr,val);
                }

            } else if(!strcmp("s",command)){
                scanf("%s %s %s",path,attr,val);
                if(!set_file_print_error(path,attr,val,strlen(val),0)){
                    print_ok_string_tuple(attr,val);
                }

            } else if(!strcmp("t",command)){
                scanf("%s %s",path,attr);
                if(!set_file_print_error(path,attr,"",0,0)){
                    print_ok_string_tuple(attr,"");
                }

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
                    print_init();
                    print_atom("ok");
                    print_end();
                }

            } else if(!strcmp("a",command)){
                scanf("%s %s %s",path,attr,val);
                char prev[256];
                int length=256;
                if( !get_file_print_error(path,attr,prev,&length,0) ) {
                    prev[length]=0;
                    strcat(prev,val);
                    if ( !set_file_print_error(path,attr,prev,strlen(prev),0)){
                        print_ok_string_tuple(attr,prev);
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

                    print_init();
                    print_tuple_header(2);
                    print_atom("ok");
                    //printf("{ok,[");
                    print_list_header(count);
                    for(i=count;i;i--) {
                    //    printf("\"%s\"",ATTR_ENTRY(buffer,i-1)->a_name);
                        print_string(ATTR_ENTRY(buffer,i-1)->a_name);
                    //    printf(i-1?",":"");
                    }
                    //printf("]}.");
                    print_list_end();
                    print_end();

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
                    print_init();
                    print_tuple_header(2);
                    print_atom("ok");
                    if(count){
                        print_list_header(count);
                    }
                    for(i=count;i;i--){
                        attrlist_ent_t* ent = ATTR_ENTRY(buffer,i-1);
                        length=256; //ent->a_valuelen;
                        if( !get_file_print_error(path,ent->a_name,val,&length,0)) {
                            val[length]=0;
                            print_string_tuple(ent->a_name,val);
                        }
                    }
                    print_list_end(); /* this is the same as for an empty list, so this works for both empty and non-empty lists */
                    print_end();

                }

            } else {
                print_error("invalid_command");
                scanf("%*[^\n]");
            }
        } else {
            print_atom_tuple_response("exit","ok");
            return 0;
        }
        fflush(NULL);

    }

}
