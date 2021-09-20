function return_array(n)
  integer :: n
  integer :: return_array(n)
  return_array = 0 
end function return_array

program test2
  integer :: array(2)
  array = return_array(2)
  write(*,*) array(:)
end program test2
