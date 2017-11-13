# genreal steps
[1] work on jenner in a <branch_id>
[2] devtools::load_all("../jenner") # with the path modified to match your computer
[3] test if changes in the first step work
[4] step 1-3 interactively until good

# problems solved
-- summary_output script: the suffix issue
-- migrate_coverage.sql: due to the bestminus activity_type in SDF7
NOTE:: to make the modified update capable of running all possible modups,
I am using RIGHT JOIN at the moment.

# problem to solve
-- migrate_coverage.sql: LEFT/RIGHT JOIN issue
    
    Left Join makes most of the sense for a modified update, because we do not intend to loss any 'old' impacts. However, there are this possibility that 'no new coverage found' is not an error, but an warning - eg. the MR_Measles.

  I tend to use RIGHT JOIN for mock modup, while LEFT JOIN for real modup. How can we deal with it?

