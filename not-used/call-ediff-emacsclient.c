/* 
 * call-ediff is used to call ediff from version control
 * gcc -Wl,--subsystem,windows -o call-ediff call-ediff-emacsclient.c
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

   char *file1 = argv[1];
   char *file2 = argv[2];
   char *file3; 
   

   hackPathSeparator(file1);
   hackPathSeparator(file2);

   if (argc > 3) {
      file3 = argv[3];
      hackPathSeparator(file3);
   }
 
   
    //argv[1] = "";
    argv[1] = "-e";
    

    char *command = (char *)malloc(4096);
    //use version that doesn't open a new frame
    //emacsclient -e "(progn (ediff \"e:/home/.bash_profile\" \"e:/home/.dave_bash_profile\") (raise-frame))"
    if (argc > 3) {
       sprintf(command, "(progn (ediff-files3 \"%s\" \"%s\" \"%s\") (raise-frame))", file1, file2, file3);
    }
    else {
       sprintf(command, "(progn (ediff \"%s\" \"%s\") (raise-frame))", file1, file2);
    }

    argv[2] = command;

    //yargv[6] = (char *)0;

    //execvp("gnuclientw.exe", argv);
    execvp("emacsclientw.exe", argv);
    return 0;
}
