#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINES 339

typedef struct {
  char* gateType;
  int outputVal;
  char* outputId;
  char* inputIds[];
} Instruction;

Instruction* parseLine(char* line);
Instruction* newInstruction(void);

int main(void){
  FILE* fp = fopen("./day-07-test.txt", "r");
  // FILE* fp = fopen("./day-07-data.txt", "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening input file.\n");
    return 1;
  }

  char* line = NULL;
  size_t read;
  size_t length = 0;
  Instruction* instructions[LINES];
  int instructionN = 0;

  while((read = getline(&line, &length, fp )) != -1){
    Instruction* i = parseLine(line);
    instructions[instructionN] = i;
    instructionN++;
  }

  Instruction* noOutValue[LINES];
  for(int i = 0; i < LINES; i++){
    noOutValue[i] = NULL;
  }

  int someNoOutValue = 0;
  for(int i = 0; i < LINES; i++){
    Instruction* current = instructions[i];
    if(current -> outputVal > 0){
    }
    if(current->outputVal == 0){
      noOutValue[i] = current;
      someNoOutValue = 1;
    }
  }

  while(someNoOutValue == 1){
    for(int i = 0; i < noOutValue; i++){
      if(noOutValue[i] != NULL){

      }
    }
  }

  fclose(fp);
  if(line){
    free(line);
  }
}

Instruction* parseLine(char* line){
  char inputs[20];
  char output[3];
  char gate[7]; // 6 for RSHIFT/LSHIFT + 1 for '\0'
  char inputIds[2][3];
  Instruction* i = newInstruction();
  sscanf(line, "%[a-zA-Z0-9 ] -> %s", inputs, output);
  if(strstr(inputs, "NOT")){
    sscanf(inputs, "%s %s", gate, inputIds[0]);
    i->gateType = gate;
    i->outputId = output;
    i->inputIds[0] = inputIds[0];
  } else if(atoi(inputs) == 0) {
    sscanf(inputs, "%s %s %s", inputIds[0], gate, inputIds[1]);
    i->gateType = gate;
    i-> outputId = output;
    i->inputIds[0] = inputIds[0];
    i->inputIds[1] = inputIds[1];
  } else {
    i->outputVal = atoi(inputs);
    i->outputId = output;
  }
  return i;
}

Instruction* newInstruction(){
  Instruction* i = malloc(sizeof(Instruction));
  i->gateType = NULL;
  i->outputId = NULL;
  i->outputVal = 0;
  i->inputIds[0] = NULL;
  i->inputIds[1] = NULL;
  return i;
}
