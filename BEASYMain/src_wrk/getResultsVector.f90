module getResultsVector_Module
    use, intrinsic :: iso_fortran_env;                
    use DataStructure_Module
    implicit none;
!    private;
    public :: DDDMS_getResultsVector;       
    
    
    type :: DDDMS_getResultsVector        
    contains 
        procedure :: getResultsVector;
    end type DDDMS_getResultsVector
    
contains   
!
!----------------------------------------------------------------------------------------------    
    subroutine getResultsVector(self,resultsData)
!----------------------------------------------------------------------------------------------    
!   
        implicit none;        
    
!   Declarations
    
        class(DDDMS_getResultsVector)             :: self;
        class(DDDMS_DataContainer)                :: resultsData;
        integer(INT32)                            :: i;
        integer(INT32)                            :: k;
        integer(INT32)                            :: iszXB;
        integer(INT32)                            :: iszUL;
        integer(INT32)                            :: iszresv;
        integer(INT32)                            :: j;        
        integer(INT32)                            :: nzones;
        integer(INT32)                            :: status;
        
        nzones=size(resultsData%ZonesData);                    
        do i = 1,nzones            
            iszXB=size(resultsData%ZonesData(i)%XB(:))
            iszresv=size(resultsData%ZonesData(i)%resVec)                        
            resultsData%ZonesData(i)%resVec(1:iszXB)=resultsData%ZonesData(i)%XB(:)
            resultsData%ZonesData(i)%resVec(iszXB+1:iszresv)=resultsData%ZonesData(i)%ULI(:)            
        enddo        
    end subroutine
!-----------------------------------------------------------------------------------------------    
    
end module getResultsVector_Module