#include <stdio.h>
#include <string.h>
#include <openssl/md5.h>
#include <stdlib.h>

char* hashString(char* str);
char* readFile(char* filename);

int main(void){
  char* buffer = readFile("./day-04-data.txt");
  char* originalBuffer = malloc(strlen(buffer));
  strcpy(originalBuffer, buffer);

  char* hash = hashString(buffer);
  int toAdd = 0;
  do {
    char toAddStr[10];
    sprintf(toAddStr, "%d", toAdd);

    int requiredSize = strlen(toAddStr) + strlen(originalBuffer);
    if(requiredSize > strlen(buffer)){
      buffer = realloc(buffer, requiredSize + 1);
    }

    for(int i = 0; i < strlen(toAddStr); i++){
      buffer[strlen(originalBuffer) + i] = toAddStr[i];
    }

    hash = hashString(buffer);
    toAdd++;
  } while(strncmp(hash, "00000", 5));

  printf("Part one: %s\n", hash);
  printf("          %d\n", toAdd - 1);

  do {
    char toAddStr[10];
    sprintf(toAddStr, "%d", toAdd);

    int requiredSize = strlen(toAddStr) + strlen(originalBuffer);
    if(requiredSize > strlen(buffer)){
      buffer = realloc(buffer, requiredSize + 1);
    }

    for(int i = 0; i < strlen(toAddStr); i++){
      buffer[strlen(originalBuffer) + i] = toAddStr[i];
    }

    hash = hashString(buffer);
    toAdd++;
  } while(strncmp(hash, "000000", 6));

  printf("Part two: %s\n", hash);
  printf("          %d\n", toAdd - 1);

  free(buffer);
  free(originalBuffer);
  return 0;
}

char* readFile(char* filename){
  FILE* fp = fopen(filename, "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening input file\n");
    exit(1);
  }
  int bufferSize = 16;
  char* buffer = malloc(bufferSize);
  int charN = 0;
  int c;
  while((c = fgetc(fp)) != EOF){
    if(charN == bufferSize - 1){
      bufferSize += 16;
      buffer = realloc(buffer, bufferSize);
      if(buffer == NULL){
        fprintf(stderr, "Error reallocating memory to buffer\n");
        exit(2);
      }
    }
    if(c != '\n'){
      buffer[charN] = c;
      charN++;
    }
  }

  if(charN == bufferSize){
    bufferSize++;
    buffer = realloc(buffer, bufferSize);
    if(buffer == NULL){
      fprintf(stderr, "Error reallocating memory to buffer\n");
      exit(2);
    }
  }
  charN++;
  buffer[charN] = '\0';

  fclose(fp);

  return buffer;
}

char* hashString(char* str){
  unsigned char digest[16];
  MD5_CTX ctx;
  MD5_Init(&ctx);
  MD5_Update(&ctx, str, strlen(str));
  MD5_Final(digest, &ctx);
  char md5String[33];
  for(int i = 0; i < 16; i++){
    sprintf(&md5String[i*2], "%02x", (unsigned int)digest[i]);
  }
  return md5String;
}
