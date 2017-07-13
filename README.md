# Reversible Erlang

An implementation of the reversible semantics for Erlang.

### Dependencies

This tool does not have any dependencies, but it uses [wx](http://erlang.org/doc/apps/wx/chapter.html), the Erlang binding of wxWidgets, that is now included in Erlang/OTP.

If you have any problem running this application, please make sure that you are using an upgraded version of Erlang/OTP (the tool has been tested with Erlang/OTP 18).

### How to use

First, compile the project:
```
make
```
Then, execute the script *rev-erlang.sh*:
```
./rev-erlang.sh
```
An astonishing graphical interface will appear in your screen.

![GUI screenshot](https://github.com/mistupv/rev-erlang/blob/screens/rev-erlang-init.png?raw=true)

To start using the application:
 1. Select an Erlang file using the File > Open option from the menu bar.
    This compiles the file and shows the Core Erlang code in the Code window.
 2. Choose the function to be evaluated and write its arguments (if needed).
 3. Push the START button.

Then, the initial state of the system will appear in the State window.

You can control the evaluation of a program using two modes:
 * **Manual**: Specify a pid and select a firable rule.
 * **Automatic**:
    * **Forward/Backward**: Given a number of steps N, perform N steps in the selected direction. The fired rules are selected at random.
    * **Normalize**: Move the system forward up to a system where the only firable rules are Sched (deliver a message). The fired rules are selected at random.

Both modes can be used interchangeably.

### Publications

This tool is an implementation of the proposed reversible semantics in these publications:
  * Naoki Nishida, Adrián Palacios and Germán Vidal. [A Reversible Semantics for Erlang](http://users.dsic.upv.es/~gvidal/german/lopstr16b/paper.pdf). To be published in *Proceedings of the 26th International Symposium on
Logic-Based Program Synthesis and Transformation*.

[comment]: # (Add pages and year once published --, 2016, pages 28:1-28:18)
