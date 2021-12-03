const fs = require("fs")
const fsp = require("fs/promises")
const path = require("path")
const util = require("util")

main()

async function main(){
  const [nodePath, selfPath, ...scriptArgs] = process.argv
  const filePath = path.resolve(scriptArgs[0])
  
  const htmlString = await fsp.readFile(filePath, "utf-8")
  const tree = buildTree(htmlString)
  console.log(util.inspect(tree.children[0].children[1].children))
  //                       root / DOCTYPE  /   html    / head + meta
  
  function buildTree(string){
    class Node {
      constructor(nodeName, parentNode = null){
        this.nodeName = nodeName
        this.children = []
        this.parentNode = parentNode
      }

      addNewChild(content){
        this.children.push(content)
      }

      get lastChild() {
        return this.children[this.children.length]
      }

      set lastChild(val){
        this.children[this.children.length] = val
      }

    }
    const tree = new Node("root")
    let tagOpen = false
    let tagName = ""
    let currentNode = tree
    let charIndex = 0
    for(let char of string)
    {
      if(tagOpen){
        // we are inside the angle brackets on a node
        if(char === "/"){
          // we are in a closing tag of the element - could be self-closing
          if(currentNode.parentNode){
            tagOpen = false
            currentNode = currentNode.parentNode
          } else {
            return tree
          }
        }
        if(char === ">"){
          if(tagOpen){
            // we have got to the end of a tag
            tagOpen = false
            handleNewTag(tagName)
            tagName = ""
          }
        } else {
          // build the tag name
          tagName += char
        }
      } else {
        // we are not inside the angle brackets - we are looking at the inner content of a tag
        if(char === "<"){
          // beginning of a new tag? How to differentiate between a tag and a less-than sign?
          if(!tagOpen){
            tagOpen = true
          }
        } else {
          // string contents of a node
          if(typeof currentNode.lastChild === "string"){
            console.log("no concat?", currentNode.lastChild + char)
            currentNode.lastChild = currentNode.lastChild + char
          } else {
            currentNode.addNewChild(char)
          }
        }
      }
      charIndex++
    }

    return tree

    function handleNewTag(tagName){
      const newNode = new Node(tagName, currentNode)
      currentNode.addNewChild(newNode)
      currentNode = newNode
    }
  }
}