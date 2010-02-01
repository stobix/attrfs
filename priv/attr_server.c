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
            if(!strcmp("fil",command)){
                int length=255;
                scanf("%s %s",path,attr);
                if( getxattr(path,attr,(char*)&val,length) == -1 ){
                    switch(errno){
                        case ENOATTR: printf("The attribute in unreachable or does not exist.\n"); break;
                        case ERANGE: printf("The buffer size is too small\n");
                                     break;
                        case ENOTSUP: printf("Extended arguments disabled or not supported on fs! (Or file missing)\n"); break;
                        default: printf("Unknown error!\n");
                    }

                } else {
                    printf("%s has value %s",attr,val);
                }



            } else {
                printf("Invalid command!\n");
            }
        } else {
            printf("Exiting...\n");
            return 0;
        }

    }

}
