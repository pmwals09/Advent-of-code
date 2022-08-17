#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int partOneTestWord(char* word);
int partTwoTestWord(char* word);

int main(void){
  FILE* fp = fopen("./day-05-data.txt", "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening input file.\n");
    exit(1);
  }

  int partOneTotalNice = 0;
  int partTwoTotalNice = 0;
  char* line = NULL;
  size_t length = 0;
  ssize_t read;
  while((read = getline(&line, &length, fp)) != -1){
    partOneTotalNice += partOneTestWord(line);
    partTwoTotalNice += partTwoTestWord(line);
  }
  printf("Part one: %d\n", partOneTotalNice);
  printf("Part two: %d\n", partTwoTotalNice);

  fclose(fp);
  if(line != NULL){
    free(line);
  }
}

int partOneTestWord(char* word){
  // does not contain ab, cd, pq, or xy, even if part of the other requirements
  char* badWords[] = {
    "ab", "cd", "pq", "xy"
  };
  for(int i = 0; i < 4; i++){
    if(strstr(word, badWords[i])){
      return 0;
    }
  }

  // contains at least 3 vowels
  char vowels[] = {
    'a', 'e', 'i', 'o', 'u'
  };
  int vowelCount = 0;

  // contains at least one doubled letter in a row
  int hasDouble = 0;

  for(int i = 0; i < strlen(word) - 1; i++){
    char c = word[i];
    for(int j = 0; j < 5; j++){
      if(c == vowels[j]){
        vowelCount++;
      }
    }

    if(word[i] == word[i + 1]){
      hasDouble = 1;
    }

    if(vowelCount > 2 && hasDouble > 0){
      return 1;
    }
  }
  for(int i = 0; i < 5; i++){
    if(word[strlen(word) - 1] == vowels[i]){
      vowelCount++;
    }
  }

  if(vowelCount > 2 && hasDouble > 0){
    return 1;
  } else {
    return 0;
  }
}

int partTwoTestWord(char* word){
  // contains a pair of any two letters that appears at least twice in the 
  // string without overlapping
  int hasDoubledPair = 0;
  // contains at least one letter which repeats with exactly one letter between
  // them
  int hasSplit = 0;
  for(int i = 0; i < strlen(word) - 2; i++){
    if(word[i] == word[i + 2]){
      hasSplit = 1;
    }

    char* pair = malloc(3);
    pair[0] = word[i];
    pair[1] = word[i + 1];
    pair[3] = '\0';

    if(strstr(word + i + 2, pair)){
      hasDoubledPair = 1;
    }
    free(pair);
  }

  if(hasDoubledPair > 0 && hasSplit > 0){
    return 1;
  }
  return 0;
}
