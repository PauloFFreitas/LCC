#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

int exec_command(char* arg){
	char* exec_args[10];
	char* string;
	int exec_ret=0;
	int i=0;
	char* command = strdup(arg);
	string = strtok(command," ");
	
	
	while(string != NULL){
		exec_args[i] = string;
		string = strtok(NULL," ");
		i++;
	}
	exec_args[i] = NULL;
	exec_ret = execvp(exec_args[0],exec_args);
	return exec_ret;
	}
		
void multiexec(int n, char* commands[]){
	
	int pipes[n-1][2];
	int status[n];
	pid_t pid;
	struct timeval inicio, fim;
	
	for(int c = 0; c < n; c++){
	
		if(c==0){ //primeiro a exec
			
			printf("Running PID %d\n", getpid());
			gettimeofday(&inicio,NULL);
			
			pipe(pipes[c]);
			int res = fork();
			if(res==0){ // filho primeiro
				close(pipes[c][0]);
				dup2(pipes[c][1],1);
				close(pipes[c][1]);
				
				exec_command(commands[c]);
				_exit(1);
			}else{	// pai do primeiro
				close(pipes[c][1]);
				}
		}else if(c==n-1){// ultimo a exec
			int res=fork();
			if(res==0){	// filho ultimo
				close(pipes[c-1][1]);
				dup2(pipes[c-1][0],0);
				close(pipes[c-1][0]);
				
				exec_command(commands[c]);
				_exit(1);
			}else{ // pai do ultimo
				close(pipes[c-1][0]);
				}
		}else{ // meio
			pipe(pipes[c]);
			int res= fork();
			if(res==0){ // filho do meio
				close(pipes[c][0]);
				dup2(pipes[c-1][0],0);
				close(pipes[c-1][0]);
				dup2(pipes[c][1],1);
				close(pipes[c][1]);
				
				exec_command(commands[c]);
				_exit(1);
			}else{ // pai dos meio
				close(pipes[c-1][0]);
				close(pipes[c][1]);
				}
			}
		}
	
	for(int c = 0; c < n;c++){
		int pid = wait(&status[c]);
		}
		
	gettimeofday(&fim,NULL);						

	long long tempo = (fim.tv_sec - inicio.tv_sec)*1000.0;			
	tempo += (fim.tv_usec - inicio.tv_usec)/1000.0;				
	
	printf("Ended in %lld ms\n\n",tempo);
	}
	
void uniexec(char arg[]){

	int fd_fifo;
	if((fd_fifo = open("fifo", O_WRONLY))==-1) perror("Server offline");
	
	pid_t res;
	int status = 0;
	struct timeval inicio,fim;
	char* prog = arg;
	char* args[32];
	int  i=0;
	char str[128];
	
	char *token = strtok(prog," ");
	while(token != NULL){
		args[i]=token;
		token=strtok(NULL," ");
		i++;
		}									
	args[i] = NULL;
	
	gettimeofday(&inicio,NULL);	
	
	res = fork();
		
	if(res==0){	
		int pid = getpid();
		long long timestamp = (inicio.tv_sec*1000 + inicio.tv_usec /1000);
		sprintf(str,"%d %s %lld i\n",pid,args[0],timestamp);
		
		int write_bytes = write(fd_fifo,str, strlen(str));
		printf("Running PID %d\n",pid);
		execvp(args[0],args);
		exit(EXIT_FAILURE);
		}
	else{
		int terminated_pid = wait(&status);
		//sleep(40);
		gettimeofday(&fim,NULL);
		long long timestamp = (fim.tv_sec*1000 + fim.tv_usec /1000);
		sprintf(str,"%d %s %lld f\n",terminated_pid, args[0],timestamp);
		
		int write_bytes = write(fd_fifo,str, strlen(str));
		
		long long tempo = (fim.tv_sec - inicio.tv_sec)*1000.0;
		tempo += (fim.tv_usec - inicio.tv_usec)/1000.0;
		
		if(WIFEXITED(status)){
	     		printf("Ended in %lld ms\n\n",tempo);
	   		}
		}
	}
	
int main(int argc, char* argv[]){
	
	int fd_fifo, fd_fifostatus;
	char buffer[128];
	
	if(argc==1){printf("Sem argumentos a executar pelo cliente.\n"); exit(1);}
	if(strcmp(argv[1],"execute")==0){
		if(argc==2){printf("Sem argumento a executar.\n"); exit(1);}
		if(strcmp(argv[2],"-u")==0){
			if(argc==3) {printf("Sem programa a executar.\n"); exit(1);}
			uniexec(argv[3]);
			}
		if(strcmp(argv[2],"-p")==0){
			if(argc==3){printf("Sem programa a executar.\n"); exit(1);}
			char* progs[20];
			int i=0;
			char* token = strtok(argv[3],"|");
			while(token != NULL){
				progs[i] = token;
				token = strtok(NULL,"|");
				i++;
				}
			multiexec(i,progs);
			}
		}

	if(strcmp(argv[1],"status")==0){
		if((fd_fifo = open("fifo", O_WRONLY)) == -1) perror("Server offline");
		
		int write_bytes = write(fd_fifo,argv[1],strlen(argv[1]));
		close(fd_fifo);
		
		sleep(1);
		
		if((fd_fifostatus = open("fifostatus", O_RDONLY)) == 0) printf("Fifo server -> client is connected.\n");
		
		int read_bytes=0;
		while ((read_bytes = read(fd_fifostatus, buffer, 128))>0){
			int write_bytes = write(1, buffer, read_bytes);
			}
		close(fd_fifostatus);
		}
	return 0;
	}
