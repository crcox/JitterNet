writeJitterNetExampleFile <- function(filename, sampleDF, headerList=NA, printFrequency=FALSE) {
    # WRITEJITTERNETEXAMPLEFILE Write a dataframe of word representations as a LENS example file.
    #
    # filename : path to where the output should be written. Extension .ex by convention.
    #
    # sampleDF : data.frame containing all the information needed to produce the example file.
    #  Required fields include:
    #    example_id   : An index for the example
    #    word         : The word, without additional numbering etc.
    #    input_onset  : The slot number where the first letter of the input should go.
    #    target_onset : The slot number where the first letter of the output should go.
    #    letter_index : A number in the range 1--26 indicating a letter a--z.
    #                   (If provided, a single example will span multiple lines, one per letter)
    #    freq         : This optional field contains frequency data for probabilistic sampling.
    #
    # headerList : A list (or named vector) of header information. Valid header fields include:
    #    proc:  <S set-proc>
    #    max:   <R set-maxTime>
    #    min:   <R set-minTime>
    #    grace: <R set-graceTime>
    #    defI:  <R set-defaultInput>
    #    actI:  <R set-activeInput>
    #    defT:  <R set-defaultTarget>
    #    actT:  <R set-activeTarget>
    #
    # printFrequency : Toggles whether data in the optional field "freq" should be written.
    #

    # HELPER FUNCTIONS
    # ================
    word2letters <- function(x) {
        if ( is.character(x) && length(x) == 1 ) {
            unlist(strsplit(x,split = ''))
        } else {
            stop('Input must be a single string.')
        }
    }
    letters2numbers <- function(x) {
        vapply(
               toupper(x),
               FUN.VALUE=numeric(1),
               FUN=function(y) {match(y,LETTERS)}
        )
    }
    reverseWord <- function(x) {
        return(paste(rev(word2letters(x)), collapse=''))
    }
    printNewlines <- function(n) {cat(rep('\n',n))}
    printHeader<- function(x) {
        for (k in names(x)) {
            cat(sprintf("%s: ", k), x[[k]], '\n', sep='')
        }
        cat(';')
    }
    printExampleName <- function(word, ind) {
        cat('name: ', word, '_', ind, '\n', sep = '')
    }
    printSparseInputBySlot <- function(x, onset) {
        n <- length(x)
        keys <- paste('slot', seq(onset,onset + n - 1), sep='')
        cat(sprintf("i: {%s} ", keys[1]), x[1] - 1, '\n', sep='')
        if ( n > 1 ) {
            for (i in 2:n) {
                cat(sprintf("   {%s} ", keys[i]), x[i]-1, '\n', sep='')
            }
        }
        cat('\n',sep='')
    }
    printSparseOutputBySlot <- function(x, onset) {
        n <- length(x);
        x <- x + (seq(onset - 1, onset + (n - 2)) * 26)
        cat("t: {output}", x-1, sep=' ')
        cat('\n',';','\n',sep='')
    }
	sink.reset <- function(){
		for(i in seq_len(sink.number())){
			sink(NULL)
		}
	}
	writeEx <- function(filename, sampleDF, headerList, printFrequency) {
        hasLetterIndex <- 'letter_index' %in% names(sampleDF)
		cat(paste("Writing to",filename))
		sink(filename)

		printHeader(headerList)
		printNewlines(2)

		example_ids <- unique(sampleDF$example_id)
		for (i in 1:length(example_ids)) {
			E <- subset(sampleDF, sampleDF$example_id == example_ids[i])
			word <- E$word[1]
			if (hasLetterIndex) {
				word_vec <- E$letter_index
			} else {
				word_letters <- word2letters(word)
				word_vec <- letters2numbers(word_letters)
			}
			input_onset <- E$input_onset[1]
			target_onset <- E$target_onset[1]

			printExampleName(word, input_onset);
			if (printFrequency) {
				printExampleFrequency(word, E$freq[1]);
			}
			printSparseInputBySlot(word_vec, input_onset)
			printSparseOutputBySlot(word_vec, target_onset)
		}
		sink()
	}

    # INPUT CHECKING
    # ==============
    if (missing(headerList)) {
        headerList <- list(
          defI = 0, #the default unit value for inputs
          actI = 1, #the default activation value for inputs
          defT = 0, #the default unit value for targets
          actT = 1 #the default activation value for targets
        )
    }

    # START WRITING
    # =============
	tryCatch(
		writeEx(filename, sampleDF, headerList, printFrequency),
		error = sink.reset
	)
}
