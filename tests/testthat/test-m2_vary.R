testthat::context('Test m2_vary function')

lambda <- runif(10, 0.60, 0.67)
objw <- m2_vary(lambda, m1 = 0.23, pc = 0.8, pt = 0.7, pd_var = 0.004, nc = 100, nt = 100, method = 'wald')
objf <- m2_vary(lambda, m1 = 0.23, pc = 0.8, pt = 0.7, pd_var = 0.004, nc = 100, nt = 100, method = 'fm')


testthat::describe('basic usage',{

  it('class objw',{
    testthat::expect_true(inherits(objw,'list'))
  })

  it('class objf',{
    testthat::expect_true(inherits(objf,'list'))
  })

  it('dim objw',{
    testthat::expect_equal(length(objw),5)
  })

  it('dim objf',{
    testthat::expect_equal(length(objf),5)
  })

})
