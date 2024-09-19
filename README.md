# stash_plot


the goal is to save off the plot object and maintain metadata on when it was created
Want to create a function that maintains a list of plot file locations, date of creation so that plots can be easily reloaded and modified or assembled
The main function would read in a qs object with a named list which contains a named vector of file locations, who created it and dates of creation
Plot objects will be saved as .qs objects into the output/figure_objs directory with the plot name as the file name and the timestamp following the format "_YYYYMMDDHHMMSS.qs"
The text file will be saved to output/figure_objs/plot_list.txt
