# Analysis of the JSON from my ChatGPT conversations -------------------------------
df <- read_json("~/main/primary/stats/donnees/GPT_prompt/conversations.json")  # Read the JSON file containing conversation data
print(df)  # Print the JSON data to inspect its structure

# List to store all the conversations.
# An index will be associated with it to track the progress of concatenations.
all_user_prompts <- list()  # Initialize an empty list to hold all conversation prompts
index <- 1  # Set the initial index for tracking the conversation entries

# Loop through each conversation in the JSON data.
# Note: Although 'conf' may seem to appear out of nowhere, we can define a variable in the for() loop.
# mapping_list will refer to the 'mapping' sublists of each conversation.
# conv_prompts is a list to store the prompt of the conversation currently being processed.
for (conv in df) {  
  mapping_list <- conv$mapping  # Extract the 'mapping' sublist from the current conversation
  conv_prompts <- list()  # Initialize an empty list to store user prompts from this conversation
  sub_index <- 1  # Set a sub-index to track the prompts within the current conversation
  
  # Iterate over the names in mapping_list.
  # This loop goes through each node name in mapping_list, which contains the relevant sublists.
  for (node_name in names(mapping_list)) {
    
    # Define variables to store values from the current node.
    # 'node' holds the data of the current node; there are many nodes, some of which are not of interest.
    # 'msg' is used to retrieve the value of node$message, which is what we want to process.
    node <- mapping_list[[node_name]]  # Retrieve the current node using its name
    msg <- node$message  # Extract the message from the current node
    
    # Some of the lists present are not of interest, so we need to filter them.
    # The first if() checks if a 'message' list exists in the node.
    # The second if() verifies that the message was written by the user (role equals "user").
    if (!is.null(msg)) {  # Check that the message exists in the current node
      if (!is.null(msg$author$role) && msg$author$role == "user") {  # Confirm that the message's author is the user
        # Combine all the text parts into a single string.
        text <- paste(unlist(msg$content$parts), collapse = " ")  # Merge text fragments into one complete string
        
        # Retrieve the creation date of the message.
        # If the creation time is not specified, assign the value NA.
        time <- msg$create_time  # Get the creation time from the message
        if (is.null(time)) time <- NA  # If time is missing, set it to NA
        
        # Create a data frame to store the message text and its creation time.
        conv_prompts[[sub_index]] <- data.frame(
          text = text,           # The full text of the message
          create_time = time,    # The time the message was created
          stringsAsFactors = FALSE  # Ensure that text is not converted to factors
        )
        # Increment the sub-index for the next prompt in the current conversation.
        sub_index <- sub_index + 1  # Increase the sub-index by one
      }
    }
  }
  
  # If there are any user prompts collected for the current conversation, bind them into a single data frame.
  if (length(conv_prompts) > 0) {  
    all_user_prompts[[index]] <- do.call(rbind, conv_prompts)  # Combine the list of data frames into one for this conversation
    index <- index + 1  # Increment the main index to move to the next conversation
  }
}

# Combine all individual conversation data frames into one final data frame.
if (length(all_user_prompts) > 0) {  
  final_user_prompts <- do.call(rbind, all_user_prompts)  # Bind all conversation data frames into one comprehensive data frame
} else {
  final_user_prompts <- data.frame(  # If no user prompts were found, create an empty data frame with the correct structure
    text = character(),       # An empty character vector for text messages
    create_time = numeric(),  # An empty numeric vector for creation times
    stringsAsFactors = FALSE  # Do not convert strings to factors
  )
}

# View the first few user prompts from the final data frame.
head(final_user_prompts)  # Display the first few rows in the console
View(final_user_prompts)  # Open the data frame in a viewer window for inspection
