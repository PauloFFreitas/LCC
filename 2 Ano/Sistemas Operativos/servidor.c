#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

ssize_t readln (int fd, char* line, size_t size){

	int read_bytes = 0, pos = 0;
	
	while(pos < size && read(fd, line+pos, 1)>0){
		read_bytes++;
		if(line[pos]=='\n'){
			break;
		}							
		pos++;
	}
	return read_bytes;
}

void status(){
	char buffer[128];
	int fd_log, fd_fifo;
	
	printf("\nStatus Mode\n");
	
	mkfifo("fifostatus",0666);
	
	if((fd_log = open("log.txt", O_CREAT | O_RDONLY , 0666)) == -1){
		perror("Open log");
	} else printf("log.txt is open for reading.\n");
	
	if((fd_fifo = open("fifostatus", O_WRONLY)) == -1){
	} else printf("Fifo server -> client is connected.\n");
	
	int read_bytes=0;
	while ((read_bytes = readln(fd_log, buffer, 128))>0){
		printf("li %d bytes.\n", read_bytes);
		char pid[5]="";
		char time[20]="";
		char prog[10]="";
		char flag[2]="";
		char* token;
		
		token = strtok(buffer," ");
		strcpy(pid,token);
		int pidconv=atoi(pid);
		
		token = strtok(NULL," ");	
		strcpy(prog,token);
		
		token = strtok(NULL," ");	
		strcpy(time,token);
		
		token = strtok(NULL," ");	
		strcpy(flag,token);
		flag[1]='\0';
		
		printf("1: Pid: %d, Nome: %s, Time: %s, Flag: %s\n",pidconv,prog,time,flag);
		if(strcmp(flag,"i")==0){
			int found = 0;
			lseek(fd_log,0,SEEK_SET);
			while((read_bytes = readln(fd_log,buffer,128))>0){
				char pid_c[5]="";
				char time_c[20]="";
				char prog_c[10]="";
				char flag_c[2]="";
		
				char* token_c = strtok(buffer," ");
				strcpy(pid_c,token_c);
				int pidcconv=atoi(pid_c);
				
				token_c = strtok(NULL," ");
				strcpy(prog_c,token_c);
				
				token_c = strtok(NULL," ");
				strcpy(time_c,token_c);
				
				token_c = strtok(NULL," ");
				strcpy(flag_c,token_c);
				flag_c[1]='\0';
				
				printf("2: Pid: %d, Nome: %s, Time: %s, Flag: %s\n",pidcconv,prog_c,time_c,flag_c);
				if((pidconv==pidcconv) && (strcmp(flag_c,flag)!=0)){
					printf("FOUND -> Pid: %d, Nome: %s, Time: %s, Flag: %s\n",pidcconv,prog_c,time_c,flag_c);
					found = 1;
					break;
					}
				}
			if(!found){
				struct timeval agora;
				char str[100];
				gettimeofday(&agora,NULL);
				long long ms=(long long)agora.tv_sec*1000LL + (long long)agora.tv_usec/1000LL;
				char* erro;
				long long iniconv=strtoll(time,&erro,10);
				long long demora = ms-iniconv;
				sprintf(str,"%d %s %lld ms\n",pidconv,prog,demora);
				int write_bytes = write(fd_fifo,str,strlen(str));
				printf("Mandei pro fifo %d bytes\n",write_bytes);
				}
			}
		}
	close(fd_fifo);
	printf("Fifo server -> client is disable.\n");
	unlink("fifostatus");
	close(fd_log);
	printf("End of Status Mode\n");
	}
	
int main(int argc, char* argv[]){
	
	int fd_fifo, fd_log, res;
	char buffer[128];
	
	mkfifo("fifo", 0666);
	
	if((fd_log = open("log.txt", O_CREAT | O_WRONLY , 0666)) == -1){
		perror("Open log");
	} else printf("log.txt is open for writing.\n");
	
	if((fd_fifo = open("fifo", O_RDONLY)) == -1){
	} else printf("Fifo client -> server is connected.\n");
	
	int garantia = open("fifo", O_WRONLY);

	int read_bytes=0;
	while ((read_bytes = read(fd_fifo, buffer, 128))>0){
		if(strstr(buffer,"status")==NULL){
		int write_bytes = write(fd_log, buffer, read_bytes);
		printf("Log updated with %d bytes.\n", write_bytes);
		}else{
		res=fork();
		if(res==0){
			status();}
		continue;
		}
	}
	
	close(fd_fifo);
	unlink("fifo");
	close(fd_log);
	return 0;
}
