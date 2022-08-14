#include <stdio.h>
#include <stdlib.h>

#define NUM_CHARS 12 // 0-9, ',', '-'

typedef struct Node {
  struct Node* children[NUM_CHARS];
  int isTerminal; // 0 or 1; boolean
  char value;
} Node;

typedef struct {
  int x;
  int y;
} Point;

Node* getNewNode(char val);
int addPoint(Point p, Node* root);
int insertVal(char *val, size_t valCount, Node* root);
Node* initTrie(void);
void cleanTrie(Node* root);

int main(void){
  FILE *fp = fopen("./day-03-data.txt", "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening file.\n");
    exit(1);
  }
  Node* partOneRoot = initTrie();
  Node* partTwoRoot = initTrie();

  Point cursor = { 0, 0 };

  Point santaCursor = { 0, 0 };
  Point roboCursor = { 0, 0 };

  int c;
  int total = 1;
  int instructionNumber = 0;
  int partTwoTotal = 0;
  while((c = fgetc(fp)) != EOF) {
    Point* whichCursor = instructionNumber % 2 == 0 ? &santaCursor : &roboCursor;
    switch(c){
      case '^':
        cursor.y += 1;
        whichCursor->y += 1;
        break;
      case '>':
        cursor.x += 1;
        whichCursor->x += 1;
        break;
      case 'v':
        cursor.y -= 1;
        whichCursor->y -= 1;
        break;
      case '<':
        cursor.x -= 1;
        whichCursor->x -= 1;
        break;
    }
    total += addPoint(cursor, partOneRoot);
    partTwoTotal += addPoint(*whichCursor, partTwoRoot);
    instructionNumber++;
  }

  printf("Part one: %d\n", total);
  printf("Part two: %d\n", partTwoTotal);
  cleanTrie(partOneRoot);
  cleanTrie(partTwoRoot);
}

Node* initTrie(){
  Node* root = NULL;
  Node* initX = getNewNode('0');
  Node* separator = getNewNode(',');
  Node* initY = getNewNode('0');
  initY->isTerminal = 1;
  initX->children[10] = separator;
  separator->children[0] = initY;
  root = initX;
  return root;
}

void cleanTrie(Node* root){
  for(int i = 0; i < NUM_CHARS; i++){
    if(root->children[i] != NULL){
      cleanTrie(root->children[i]);
    }
  }
  free(root);
}

Node* getNewNode(char val){
  Node *newNode = (struct Node*)malloc(sizeof(Node));
  for(int i = 0; i < NUM_CHARS; i++){
    newNode->children[i] = NULL;
  }
  newNode->isTerminal = 0;
  newNode->value = val;
  return newNode;
}

int addPoint(Point p, Node* root){
  char pointStr[15];
  sprintf(pointStr, "%d,%d", p.x, p.y);
  int valCount = 0;
  while(pointStr[valCount] != '\0'){
    valCount++;
  }
  return insertVal(pointStr, valCount, root);
}

int insertVal(char *val, size_t valCount, Node* root){
  Node* current = root;
  for(int i = 0; i < valCount; i++){
    char item = val[i];
    int itemIndex;
    if(item == '-'){
      itemIndex = 10;
    } else if(item == ','){
      itemIndex = 11;
    } else {
      itemIndex = (int)(item) - 48;
    }
    Node* existingVal = current->children[itemIndex];
    if(existingVal == NULL){
      Node* newNode = getNewNode(item);
      current->children[itemIndex] = newNode;
      current = newNode;
    } else {
      current = current->children[itemIndex];
    }
  }

  if(current->isTerminal == 1){
    return 0;
  } else {
    current->isTerminal = 1;
    return 1;
  }
}
