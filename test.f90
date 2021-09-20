function choose(n, k) result(res)  
    implicit none
    integer, intent (in) :: n
    integer, intent (in) :: k
    integer :: res
    integer :: i, k_  
    
    res = 1
    k_ = k 
    if(k > n - k) then
      k_ = n - k
    endif
    
    do i = 0, k_ - 1
      res = res*(n - i)
      res = res/(i + 1)
    enddo
end function choose
   
  function sum_choose(num_pos, high_order, pos_val) result(res)
    implicit none 
    integer, intent(in) :: num_pos 
    integer, intent(in) :: high_order
    integer, intent(in) :: pos_val
    integer :: res  
    integer :: i
    integer :: choose 
    
    res = 0
    if(pos_val == 0) then
      res = res + choose(high_order + num_pos - 1, num_pos - 1)
    else 
      do i = 0, pos_val - 1 
        res = res + choose(high_order + num_pos - 1 - i, num_pos - 1) 
      end do
    endif 

  end function sum_choose
   
  function powers_from_index(Nmodes, Nindex) result(polyad) 
      implicit none 
      integer, intent(in) :: Nmodes, Nindex
      integer :: powers(Nmodes)   
      integer :: polyad, max_index, diff, j, term_val, remaining_order 
      integer :: choose, sum_choose
    
      polyad = -1
      max_index = 0
      powers = 0
      do while(max_index < Nindex)
        polyad = polyad + 1
        max_index = choose(Nmodes + polyad, Nmodes)
      end do
      if(polyad == 0) then
        diff = Nindex - 1
      else 
        diff = Nindex - choose(Nmodes + polyad - 1, Nmodes)
      endif 
      remaining_order = polyad
      do j = 1, Nmodes - 1 
        if(remaining_order == 0) then
          powers(j) = 0
          continue 
        else 
            term_val = 1
            do while(diff - sum_choose(Nmodes - j, remaining_order, term_val) > 0)
              term_val = term_val + 1
            end do
            term_val = term_val - 1 
            powers(j) = term_val
            if( term_val > 0) then 
              diff  = diff - sum_choose(Nmodes - j, remaining_order, term_val)
            endif
            remaining_order = remaining_order - term_val
        endif 
      end do 
      powers(Nmodes) = remaining_order
      write(*,*) powers
   end function powers_from_index 
   !
program test
  integer, dimension(3,3) :: a
  write(*,*) size(a) 
end program test
