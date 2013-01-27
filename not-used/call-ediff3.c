/* 
 * call-ediff is used to call ediff from version control
 * gcc -Wl,--subsystem,windows -o call-ediff call-ediff.c
 */
#include <stdio.h>
#include <stdlib.h>

void hackPathSeparator(char *windozePath) {
    char *c;

    for (c = windozePath; *c; c++) {
	if ('\\' == *c)
	    *c = '/';
    }
}

int main(int argc, char *argv[], char *envp[]) {

    char *param1 = argv[1];
    char *param2 = argv[2];
    char *param3 = argv[3];

    /* We get \ part separators from Perforce; hack these to / */

    hackPathSeparator(param1);
    hackPathSeparator(param2);
    hackPathSeparator(param3);


    /* hack the arg list (ugh) */

    //argv[1] = "";
    argv[1] = "-e";
    
    /* Sample usage: argv[2] = "(progn (message \"foo\") (raise-frame))"; */

    char *command = (char *)malloc(4096);
    //uscae version that doesn't open a new frame
    sprintf(command, "(progn (ediff3 \"%s\" \"%s\" \"%s\") (raise-frame))", param1, param2, param3);

    //printf("\n %s \n", command);

    //sprintf(command, "(progn (make-frame-command) (ediff \"%s\" \"%s\") (raise-frame))", param1, param2);

    argv[2] = command;
    
    //execvp("gnuclientw.exe", argv);
    execvp("emacsclientw.exe", argv);
    
    return 0;
}
