testthat::context('Test cutvul function')

res1 <- cutval(0.025, 6, 6/24)
res2 <- cutval(0.025, 5, 1)

testthat::describe('basic usage',{

  it('class res1',{
    testthat::expect_true(inherits(res1,'numeric'))
  })

  it('class objf',{
    testthat::expect_true(inherits(res2,'numeric'))
  })

  it('values res1',{
    testthat::expect_equal(round(res1,2), 2.25)
  })

  it('values res1',{
    testthat::expect_equal(round(res2,2), 2.56)
  })

})
