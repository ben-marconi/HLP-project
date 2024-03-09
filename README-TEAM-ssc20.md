# Team Contributions

##

I was allocated to create the build function D1 where where we adjust parallel wires for single connect components for better visibilty.

Created a `parellelWires` function that checks if any wire is parallel based on the specifications. (Working)

`getAllSingleConnectedSymbols1` function gets all the source target symbols that have only one port connection to a wire.

`getAllSymbols` gets all the components that have only 1 input or outout port connected to another component and if they meet the parallel wire criteria (have 3 bends in the wire segments).

`getSymbolsBoundingBox` gets the bounding boxes of all the symbols from `getAllSymbols` in order to get the positions that are needed to tranform the symbol later on.

`sheetAlignScale` function gets all symbols that meet the single wire connected component critera and need to have their wires straighten, it pairs the source to the target componenet and changes the source's position to the target's position. Further work and testing needs to be done to fully fix this and count for possible edge cases.

##
