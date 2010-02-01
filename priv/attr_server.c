#include<stdio.h>
#include<string.h>
#include<sys/types.h>
#include<attr/xattr.h>
#include<attr/attributes.h>
#include<errno.h>

int list_file_print_error(const char* path, char* buffer, int length, int options, attrlist_cursor_t* cursor){
    if( attr_list(path,buffer,length,options,cursor) == -1){
        switch(errno){
            default: printf("{error,undef}.\n");
        }
        return -1;
    } else {
        return 0;
    }
}

int get_file_print_error(const char* path, const char* attr, char* val, int* length, int options){
    if( attr_get(path,attr,val,length,options) == -1 ){
        switch(errno){
            case ENOATTR: printf("The attribute in unreachable or does not exist.\n"); break;
            case ERANGE: printf("The buffer size is too small\n");
                         break;
            case ENOTSUP: printf("Extended arguments disabled or not supported on fs! (Or file missing)\n"); break;
            default: printf("{error,undef}.\n");
        }
        return -1;
    } else {
        return 0;
    }
}

int set_file_print_error(const char* path,const char* attr, const char *val,int length,int options){
    if (attr_set(path,attr,val,length,options) == -1){
        switch(errno){
            case E2BIG: printf("{error,too big}.\n"); break;
            default: printf("{error,undef}.\n"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

int remove_file_print_error(const char* path, const char* attr, int options){
    if(attr_remove(path,attr,options) == -1){
        switch(errno){
            case ENOATTR: printf("{error, not an argument}.\n"); break;
            default: printf("{error,undef}.\n"); break;
        }
        return -1;
    } else {
        return 0;
    }
}

main(){
    char path[1024],attr[256],val[256],command[1289]="attr -g ",output[1024]; /* Should be enough for anyone! */
    FILE* fifo;
    for(;;){
        printf("Input: ");
        scanf("%s",command);
        if(strcmp("exit",command)){
            if(!strcmp("get",command)){
                int length=255;
                scanf("%s %s",path,attr);
                if(!get_file_print_error(path,attr,val,&length,0)){
                    val[length]=0;
                    printf("{ok,{\"%s\",\"%s\"}}.\n",attr,val);
                }

            } else if(!strcmp("set",command)){
                scanf("%s %s %s",path,attr,val);
                if(!set_file_print_error(path,attr,val,strlen(val),0)){
                    printf("{ok,{\"%s\",\"%s\"}}.\n", attr, val);
                }

            } else if(!strcmp("touch",command)){
                scanf("%s %s",path,attr);
                if(!set_file_print_error(path,attr,"",0,0)){
                    printf("{ok,{\"%s\",\"%s\"}}.\n", attr, "");
                }

            } else if(!strcmp("rm",command)){
                scanf("%s %s",path,attr);
                if(!remove_file_print_error(path,attr,0)){
                    printf("ok.\n");
                }

            } else if(!strcmp("append",command)){
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

            } else if(!strcmp("list",command)){
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
                    printf("].\n");

                }

            } else if(!strcmp("show",command)){
                int length=2048;
                char buffer[length];
                attrlist_cursor_t cursor;
                scanf("%s",path);
                if( !list_file_print_error(path,buffer,length,0,&cursor)){
                    attrlist_ent_t *ent_t;
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
                printf("{error,invalid_command}.\n");
                scanf("%*[^\n]");
                /* TODO: how to flush incoming char buffer? */
            }
        } else {
            printf("{exit,ok}.\n");
            return 0;
        }

    }

}
