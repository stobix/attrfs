#include<stdio.h>
#include<string.h>
#include<sys/types.h>
#include<attr/xattr.h>
#include<attr/attributes.h>
#include<errno.h>

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
                if( attr_get(path,attr,val,&length,0) == -1 ){
                    switch(errno){
                        case ENOATTR: printf("The attribute in unreachable or does not exist.\n"); break;
                        case ERANGE: printf("The buffer size is too small\n");
                                     break;
                        case ENOTSUP: printf("Extended arguments disabled or not supported on fs! (Or file missing)\n"); break;
                        default: printf("Unknown error!\n");
                    }

                } else {
                    val[length]=0;
                    printf("%s has value %s, and length %d\n",attr,val,length);
                }



            } else if(!strcmp("set",command)){
                scanf("%s %s %s",path,attr,val);
                if (attr_set(path,attr,val,strlen(val),0) == -1){
                    switch(errno){
                        case E2BIG: printf("too big\n"); break;
                        default: printf("unknown\n"); break;
                    }
                } else {
                    printf("%s now has value %s\n", attr, val);
                }

            } else if(!strcmp("list",command)){
                int length=2048;
                char buffer[length];
                attrlist_cursor_t cursor;
                scanf("%s",path);
                if( attr_list(path,buffer,length,0,&cursor) == -1){
                    switch(errno){
                        default: printf("error,error,error...\n");
                    }
                } else {
                    attrlist_ent_t *ent_t;
                   __int32_t i;
                   for(i=((attrlist_t*)buffer)->al_count;i;i--) 
                       printf("%s %d\n",ATTR_ENTRY(buffer,i-1)->a_name,ATTR_ENTRY(buffer,i-1)->a_valuelen);

                }

            } else if(!strcmp("show",command)){
                int length=2048;
                char buffer[length];
                attrlist_cursor_t cursor;
                scanf("%s",path);
                if( attr_list(path,buffer,length,0,&cursor) == -1){
                    switch(errno){
                        default: printf("error,error,error...\n");
                    }
                } else {
                    attrlist_ent_t *ent_t;
                   __int32_t i;
                   attrlist_t *list = (attrlist_t*) buffer;
                   __int32_t count = list->al_count,
                             moar = list->al_more;
                   printf(count?"{":"{}\n");
                   for(i=count;i;i--){
                       attrlist_ent_t* ent = ATTR_ENTRY(buffer,i-1);
                       length=256; //ent->a_valuelen;
                       if( attr_get(path,ent->a_name,val,&length,0) == -1 ){
                           switch(errno){
                               case ENOATTR: printf("The attribute in unreachable or does not exist.\n"); break;
                               case ERANGE: printf("The buffer size is too small\n");
                                            break;
                               case ENOTSUP: printf("Extended arguments disabled or not supported on fs! (Or file missing)\n"); break;
                               default: printf("Unknown error!\n");
                           }
                       } else {
                           val[length]=0;
                           printf("{\"%s\",\"%s\"}",ent->a_name,val);
                           printf((i-1)?",":"}\n");
                       }
                   }

                }

            } else {
                printf("Invalid command!\n");
                scanf("%*[^\n]");
                /* TODO: how to flush incoming char buffer? */
            }
        } else {
            printf("Exiting...\n");
            return 0;
        }

    }

}
