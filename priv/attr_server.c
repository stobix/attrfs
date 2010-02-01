#include<stdio.h>
#include<string.h>
#include<attr/attributes.h>
main(){
    char path[1024],attr[256],command[1289]="attr -g ",output[1024]; /* Should be enough for anyone! */
    FILE* fifo;
    for(;;){
        printf("Input: ");
        scanf("%s",command);
        if(strcmp("exit",command)){
            if(!strcmp("fil",command)){
                scanf("%s %s",path,attr);
                strcpy(command,"attr -g");
                strcat(command,attr);
                strcat(command," ");
                strcat(command,path);
                printf("%s\n",command);
                if( (fifo=popen(command,"er"))!=NULL){
                    fscanf(fifo,"%*[^\n]\n%[^\n]",&output);
                    printf("Return val:%s\n",output);
                    pclose(fifo);
                    output[0]=0;
                    *command=0;
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
