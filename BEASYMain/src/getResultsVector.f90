module getResultsVector_Module
    use, intrinsic :: iso_fortran_env;                
    use DataStructure_Module
    implicit none;
    private;
    public :: DDDMS_getResultsVector;    
    
    type :: DDDMS_ResultsVar
        real(REAL64), allocatable :: ResultsVar(:)
    end type DDDMS_ResultsVar

    !interface DDDMS_ResultsVar
    !    module procedure NewDDDMS_ResultsVar;       
    !end interface DDDMS_ResultsVar

    type :: DDDMS_getResultsVector
        type(DDDMS_ResultsVar), allocatable :: ZonalResults(:);
    contains 
        procedure :: getResultsVector;
    end type DDDMS_getResultsVector
    
    !interface DDDMS_getResultsVector
    !    module procedure NewDDDMS_getResultsVector;       
    !end interface DDDMS_getResultsVector    
    
contains   

    !type(DDDMS_getResultsVector) function NewDDDMS_getResultsVector(self)
    !
    !    implicit none;        
    !    class(DDDMS_getResultsVector) :: self;       
    !    
    !end function NewDDDMS_getResultsVector
    !
    !type(DDDMS_ResultsVar) function NewDDDMS_ResultsVar(self)
    !
    !    implicit none;        
    !    class(DDDMS_ResultsVar) :: self;       
    !    
    !end function NewDDDMS_ResultsVar
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
            iszUL=size(resultsData%ZonesData(i)%ULI(:))
            iszresv=(iszXB+iszUL)
            allocate( self%ZonalResults(i)%ResultsVar(iszresv), STAT = status)            
            self%ZonalResults(i)%ResultsVar(1:iszXB)=resultsData%ZonesData(i)%XB(:)
            self%ZonalResults(i)%ResultsVar(iszXB+1:iszresv)=resultsData%ZonesData(i)%ULI(:)
        enddo
        
    end subroutine
!-----------------------------------------------------------------------------------------------    
    
end module getResultsVector_Module