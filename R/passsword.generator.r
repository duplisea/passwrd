#' A password generator and safe
#'
#' @param master your master password. Must be numerical with however many digits you want e.g. 7812362029
#' @param sites the names of things for which the password will be generated and saved
#' @param changes a binary vector to describe if you want change a password or not (same length as sites)
#' @param how.many.letters number of lower case letters in your passwords
#' @param how.many.LETTERS number of upper case letters in your passwords
#' @param how.many.numbers number of numbers in your passwords
#' @param how.many.special number of special characters in your passwords
#' @keywords password generator
#' @export
#' @examples
#'  master.password=29852758
#'  passwords(master.password,sites=c("Work","Bank","Investment","gmail.com","yahoo.com","hotmail.com","visa","spotify"),changes=c(1,1,1,1,1,1,2,1))
passwords = function(master, sites = c("work email",
    "online bank", "gmail"), changes = c(1, 1, 1, 1, 1, 1), how.many.letters = 9,
    how.many.LETTERS = 2, how.many.numbers = 2, how.many.special = 2) {
    set.seed(master)
    special = c("!", "@", "#", "$", "+", "-", "=", "<", ">",
        "(", ")")
    pwd.vector = vector()
    #generate 10000 passwords
    for (i in 1:10000) {
        #select letters, numbers, special depending on desired quantity of each
        lett.seq = sample(letters, how.many.letters, replace = T)
        LETT.seq = sample(LETTERS, how.many.LETTERS, replace = T)
        number.seq = sample((0:9), how.many.numbers, replace = T)
        special.seq = sample(special, how.many.special, replace = T)
        # shuffle the randomly chosen characters so that they are interspersed in
        # the password
        pwd.chars = c(lett.seq, LETT.seq, number.seq, special.seq)
        pwd.seq = sample(pwd.chars)
        # collapse each password sequence into a simple vector of length 1, then
        # append vector with each new pwd
        pwd = paste(pwd.seq, collapse = "")
        pwd.vector[i] = pwd
    }
    # choose only as many passwords as there are sites * 30 changes
    set.seed(sqrt(master))
    passwords = sample(pwd.vector, length(sites) * 30, replace = F)
    pwds = list()
    istart = 1
    for (ii in 1:length(sites)) {
        iend = ii * 30
        pwds[[ii]] = passwords[istart:iend][changes[ii]]
        istart = iend + 1
    }
    names(pwds) = sites
    pwds
}
