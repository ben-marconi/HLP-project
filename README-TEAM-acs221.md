## Contributions To Team Section

My allocation for the group was to begin implementing D3 - sheetWireLabelSymbol. Given as a team the implementation would best when working with my teammate on the testing, my contribution on this individual phase was to introduce the template for the final model, ideas about the possible structure for the team and to introduce a series of helper functions ready for implementing first phase testing. The helper functions written are outlined below:
- getConnectedSymbols: Since we are replacing wires with the respective wire labels, this function allows us to easily find the symbols at either side of a wire
- getCompPortNameAndPos: Find the ids and positions on either the input or output sides of a given symbol. This is to help provide the information for wire label naming and positioning
- placeWireSymbol: To help us place a new wire symbol on the sheet
- placeWire: For placing a new wire between the newly placed wire label and the original symbol
- replaceWireWithLabelOnOutput: first version of function to place a wire label at each port. A simple version to be fully constructed during tested.
- replaceLongWiresWithLabels: The shell for the top level function used in this.


Note that the helper functions are mostly written to break down the steps of placing a wire label. They are oversimplified and when I decide on the final structure with my testing partner, they will be streamlined and adjusted to fit better with the Isse Coding Guidlines. 