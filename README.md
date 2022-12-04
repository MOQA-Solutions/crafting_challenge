
# Getting Started

This application represents a proposition to resolve Crafting Software Coding Challenge

#API

Module `crafting_challenge` export 4 functions which are the application's API:<br>
- `sort_example/0`: sort the binary *json* file's tasks which is given in the *pdf* file.
  - You can check the file [here]().
- `script_example/0`: create the expected *bash script* from the *json* file which is given in the *pdf* file.
- `sort/1`: sort any binary *json* file's tasks, the file should be given as an argument.
- `script/1`:create the expected *bash* script from any *json* file, the file should be given as an argument. 

# Example
```
git clone https://github.com/AbdelghaniMohammed/crafting_challenge
cd crafting_challenge
rebar3 shell
crafting_challenge:sort_example().
%%%% Result %%%%
crafting_example:script_example().
%%%% Result %%%%
BinJson1 = <<"....">>.
<<"....">>
crafting_challenge:sort(BinJson1).
%%%% Result %%%%
crafting_challenge:script(BinJson1).
%%%% Result %%%%
BinJson2 = <<"....">>.
<<"....">>
crafting_challenge:script(BinJson2).
%%%% Result %%%%
```
# Complexity
## Sort Function
```
C_sort = C1 + C2

C1 = O(N) in the best case
C1 = O(N!) in the worst case
```
N = Number of tasks
```
C2 = Complexity of lists:sort/1
```
## Script Function
```
C_script = C_sort + O(N)
``` 
  
