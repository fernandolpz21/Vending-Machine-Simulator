# Vending-Machine-Simulator
### About the Problem Situation:
The program in Racket is a simulator of a vending machine
that you can have various products of different prices and that you only accept payments with coins, having the ability to give change if a payment requires it.  

The simulation consists of processing sales transactions in a
text file with their data. Each sales transaction is identified with
the letter of the product to buy and the sequence of coins that are deposited for the
purchase. When processing a transaction, the program indicates with signs on the screen if the purchase is made and the remaining coins in case there are any.  

Each transaction updates the corresponding inventories. When a transaction is not possible, a message indicates the situation, for example, a sale not possible due to lack of product, or a surplus of coins not feasible to deliver.  

When the program has carried out all the sales transactions in the
input, displays the following results:
- Profit obtained after all sales transactions.
- Products with little or no inventory.
- Denominations of coins that have the repository full or almost full.
- Coin denominations that have little or no inventory.

For now, there is no interface to modify inventories through the
program, so any inventory adjustments for simulation testing should be done
directly in the machine data list.
