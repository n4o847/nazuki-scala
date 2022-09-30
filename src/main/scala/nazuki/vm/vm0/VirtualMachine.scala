package nazuki.vm
package vm0

/** 使い方:
  * ```
  * val vm = new VirtualMachine
  * vm.doScan(signed = true)
  * vm.doScan(signed = true)
  * vm.doAdd()
  * vm.doPrint(signed = true)
  * val code = vm.result
  * ```
  *
  * 使い方 2:
  * ```
  * val vm = new VirtualMachine {
  *   doScan(signed = true)
  *   doScan(signed = true)
  *   doAdd()
  *   doPrint(signed = true)
  * }
  * val code = vm.result
  * ```
  */
class VirtualMachine
    extends Alpha
    with Beta
    with binary32.Stack
    with binary32.IO
    with binary32.Arithmetic {}
