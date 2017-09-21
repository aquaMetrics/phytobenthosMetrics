## How to contribute code (DRAFT)

Hello and a warm welcome to guidelines for contributing code.

To help keep this code repository easy to maintain and the code easy to follow, **please take the time to read and follow these guidelines**.

### All changes to code should follow this route:

#### Open issue

- Open an issue and describe what your update is planning to address
- It's usually helpful to include a work exampled of the update or issue
- If you are not a maintainer, wait for feedback and comments - maybe there are different approaches that need exploring

#### Create new branch

- Once the issue is discussed, then create a new branch called 'issue-1' on github
- Pull to Rstudio and Checkout branch 'issue-1' branch
- Make changes
- Run checks and tests locally - then commit (this includes standard [lintr](https://github.com/jimhester/lintr) rules with exception that camel case is used for function/variable names e.g. 'coolFunction' not 'cool_function')
- Recommend downloading lintr from github. The CRAN package appears out of date and doesn't work on Windows:  
`library(devtools)`   
`install_github(jimhester/lintr)`  

#### Commit

- Commit message should reference the issue number e.g. 'docs: closes #1'
- Not sure how to write a commit message? Try to use this [commit message guidance](https://gist.github.com/stephenparish/9941e89d80e2bc58a153#subject-line), although this is not enforced (yet).
- Push changes to remote branch 

#### Pull request

- On github create a Pull Request to master branch from your issue-1 branch
- Wait for a maintainer to review, they will check no tests break, and the code has been tested if required
- Change are merged to master and the 'test-1' branch deleted.




