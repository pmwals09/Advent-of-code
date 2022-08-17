#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int row;
  int col;
} P;

typedef struct {
  char* action;
  P startP;
  P endP;
} I;

I parseTokensToInstruction(char* tokens[]);
P parsePoint(char* pString);

int main(void){
  FILE* fp = fopen("./day-06-data.txt", "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening input file.\n");
    return 1;
  }
  char* line = NULL;
  size_t length = 0;
  size_t read;
  int partOneLights[1000][1000];
  int partTwoLights[1000][1000];
  for(int row = 0; row < 1000; row++){
    for(int col = 0; col < 1000; col++){
      partOneLights[row][col] = 0;
      partTwoLights[row][col] = 0;
    }
  }
  while((read = getline(&line, &length, fp)) != -1){
    char* tokens[5];
    int numTokens = 0;
    char* token = strtok(line, " ");
    while(token){
      tokens[numTokens] = token;
      numTokens++;
      token = strtok(NULL, " ");
    }

    I instruction = parseTokensToInstruction(tokens);

    for(int row = instruction.startP.row; row <= instruction.endP.row; row++){
      for(int col = instruction.startP.col; col <= instruction.endP.col; col++){
        int *partOneEl = &partOneLights[row][col];
        int *partTwoEl = &partTwoLights[row][col];
        if(strcmp(instruction.action, "on") == 0){
          *partOneEl = 1;
          *partTwoEl += 1;
        } else if(strcmp(instruction.action, "off") == 0){
          *partOneEl = 0;
          *partTwoEl = *partTwoEl == 0 ? 0 : *partTwoEl - 1;
        } else if(strcmp(instruction.action, "toggle") == 0){
          *partOneEl = *partOneEl == 1 ? 0 : 1;
          *partTwoEl += 2;
        }
      }
    }
  }

  int partOneTotal = 0;
  int partTwoTotal = 0;
  for(int row = 0; row < 1000; row++){
    for(int col = 0; col < 1000; col++){
      partOneTotal += partOneLights[row][col];
      partTwoTotal += partTwoLights[row][col];
    }
  }

  printf("Part one: %d\n", partOneTotal);
  printf("Part two: %d\n", partTwoTotal);
  fclose(fp);
  if(line){
    free(line);
  }
}

I parseTokensToInstruction(char* tokens[]){
  char* action;
  char* startPString;
  char* endPString;
  if(strcmp(tokens[0], "turn") == 0){
    action = tokens[1];
    startPString = tokens[2];
    endPString = tokens[4];
  } else {
    action = tokens[0];
    startPString = tokens[1];
    endPString = tokens[3];
  }

  P startP = parsePoint(startPString);
  P endP = parsePoint(endPString);
  I instruction;
  instruction.action = action;
  instruction.startP = startP;
  instruction.endP = endP;
  return instruction;
} 

P parsePoint(char* pString){
  char* pTokenStrings[2];
  char* pToken = strtok(pString, ",");
  int numPTokens = 0;
  while(pToken){
    pTokenStrings[numPTokens] = pToken;
    numPTokens++;
    pToken = strtok(NULL, ",");
  }

  P point;
  point.row = atoi(pTokenStrings[0]);
  point.col = atoi(pTokenStrings[1]);
  return point;
}
