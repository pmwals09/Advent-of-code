const fs = require("fs")
const fsp = require("fs/promises")
const path = require("path")

main()

async function main(){
  const [nodePath, selfPath, ...scriptArgs] = process.argv
  const filePath = path.resolve(scriptArgs[0])
  
  const htmlString = await fsp.readFile(filePath, "utf-8")
  const tree = buildTree(htmlString)
  
  function buildTree(string){
    const tree = {}
    let node = {
      tagOpened: false,
      tagClosed: false
    }
    for (const char of string) {
      // tag opening
      if(char === "<"){
        // this must be the closing tag
        if(node.type){

        } else {
          node.type === ""
        }
      }
      // tag closing
      if(char === ">"){
      }
      else {

      }
    }
  }
}