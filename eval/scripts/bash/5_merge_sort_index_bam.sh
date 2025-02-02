module load samtools

samtools merge \
  data/processed/SCRNA_240430_240621/merged.bam \
  data/processed/SCRNA_240430/append.bam \
  data/processed/SCRNA_240621/append.bam
  
samtools sort -@ 32 data/processed/SCRNA_240430_240621/merged.bam > data/processed/SCRNA_240430_240621/merged_sorted.bam

samtools index data/processed/SCRNA_240430_240621/merged_sorted.bam
