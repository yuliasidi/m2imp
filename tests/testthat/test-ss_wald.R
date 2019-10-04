testthat::context('Test sample size calculation for wald')

obj <- ss_wald(0.025, 0.8, 0.8, 0.8, 0.1)

testthat::describe('basic usage',{

  it('class obj',{
    testthat::expect_true(inherits(obj,'numeric'))
  })

})
