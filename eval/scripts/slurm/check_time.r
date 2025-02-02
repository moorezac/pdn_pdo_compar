# retrieve time of file creation
file_creation <- file.info("eval/scripts/slurm/vast_to_stornext.txt")$ctime

# diff. between then and now
diff_day <- difftime(
  time1 = Sys.time(),
  time2 = file_creation,
  units = "days"
) |>
  as.numeric() |>
  format(digits = 2)

# print
message(
  paste(
    # for added effect
    rep(
      x = paste("last sync to /stornext was", diff_day, "days ago\n"),
      times = 5
    )
  )
)

# force sync if over
if (as.numeric(diff_day) > 10) {
  message(
    rep(
      x = "force sync because last sync geq. 10 days ago\n",
      times = 5
    )
  )
  system2(
    command = "sbatch",
    args = "eval/scripts/slurm/rsync_vast_to_stornext.slurm"
  )
}
