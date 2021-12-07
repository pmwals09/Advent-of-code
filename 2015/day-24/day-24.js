
  const fsp = require("fs/promises")
  const path = require("path")
  
  main()
  
  async function main(){
    const input = await parseInput()
    const packages = input.map(ea => Number(ea))

    console.log("Part one:", balancingAct(3))
    console.log("Part two:", balancingAct(4))

    function balancingAct(numCompartments){
      const target = packages.reduce((out, curr) => out + curr) / numCompartments;
      const bundles = makeBundles(packages, target);
      return minQe(bundles);
    }

    function makeBundles(array, target, arrEnd = array.length) {
      let allBundles = [];
      for (let i = arrEnd; i >= 0; i--) {
        // base case
        if (array[i] === target) {
          allBundles.push([array[i]]);
        } else if (target - array[i] > 0) {
          const subsetTarget = target - array[i];
          const subsetArrayEnd = i - 1;
          let completions = makeBundles(array, subsetTarget, subsetArrayEnd);
          const subsets = completions.map(subset => [...subset, array[i]])
          allBundles = [...allBundles, ...subsets]
        }
      }

      // deduplicate and only return set of shortest packages
      const uniqBundles = new Set()
      allBundles.map(set => set.sort().toString()).forEach(setString => uniqBundles.add(setString))
      let bundles = Array.from(uniqBundles).map(ea => ea.split(",").map(ea => Number(ea)))
      return bundles.sort((a, b) => a.length - b.length).filter((bundle) => bundle.length === bundles[0].length);
    }

    function minQe(bundles){
      let minQe = Infinity
      for(let bundle of bundles){
        bundleQe = getQe(bundle);
        if(bundleQe < minQe){
          minQe = bundleQe
        }
      }

      return minQe
    }

    function getQe(bundle){
      return bundle.reduce((out, curr) => out * curr, 1)
    }
  
    async function parseInput(){
      const rawInput = await fsp.readFile(path.resolve(__dirname, "day-24-input.txt"), "utf-8")
      const input = rawInput.split("\n")
      return input
    }
  }
        