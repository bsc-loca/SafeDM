# SafeDM

SafeDM is a hardware IP core whose functionality is to determine whether there is diversity between two redundant processors. For more information look at SafeDM documentation in the *docs* folder.

The hardware in the branch *develop* has been integrated into the De-RISC platform to validate that SafeDM works correctly. For that purpose, SafeDM includes a module dedicated to gathering statistics and generating histograms. Each histogram contains information of the difference between the signatures of both cores for a range of staggering (difference of instructions between the cores). The integration of this repo inside the De-RISC platform could be found in the branch *ts/Diversity_no_bp* of the De-RISC internal BSC repo --> (https://gitlab.bsc.es/caos_hw/derisc/derisc/-/tree/ts/Diversity_no_bp)

In the branch *release* all the logic devoted to debugging the hardware and checking its functionality is deleted. Only the logic needed for a real application is kept. Hence, only the logic used to generate and compare the signatures is kept. 



---

### Future work

* **Change how registers behave in the FIFO:**  Since there are several read and write ports in the file register, we should not keep the last read/write value until a new value is written or read and enters the FIFO. If we do it like that, we could be accounting for values that have already left the pipeline and they cannot produce diversity. To solve this issue, we should leave the FIFO position with an "empty" value each cycle that no data is read/written on its corresponding port.

* **Add write ports:**  It could improve the accuracy of the metric. However, the problem with write ports is that *write back* is the last stage of the pipeline, so the values stored in the FIFO leave the pipeline the next cycle the pipeline is not stalled. Therefore, the signature accounts for data values that are not in the pipeline anymore. This could lead to a false negative. 

* **registers containing the PC values always differ across cores:**  Each pipeline stage contains a register containing the PC of the instruction. How many different bits between the state of both redundant cores are needed to determine that there is enough diversity.

