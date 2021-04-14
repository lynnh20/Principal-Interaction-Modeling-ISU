#!/bin/bash

for i in {1..10}
do
	sbatch Two_stage_main.sh $i
done

