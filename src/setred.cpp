
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List setred_loop(int nlabelednew,int nlabeledold, NumericMatrix D, NumericVector labeled){

  List ady(nlabelednew) ;


  for (int i = 0; i < nlabelednew;++i){
    ady[i] = NumericVector::create();
  }

  for (int i = nlabeledold + 1; i <= nlabelednew;++i){

    for (int j = 1; j <= (i-1);++j){
      bool con = true;

      for (int k = 1; k <= nlabelednew;++k){
        double maximo = std::max(D(labeled[i-1]-1, labeled[k-1]-1), D(labeled[k-1]-1, labeled[j-1]-1));


        if (k != i && k != j && D(labeled[i-1]-1, labeled[j-1]-1) > maximo){

          con = false;

          break;
        }

      }
      if (con == true) {

        NumericVector tempVector = ady[i-1];
        tempVector.push_back(j);

        ady[i-1] = tempVector;

        NumericVector tempVector_dos = ady[j-1];
        tempVector_dos.push_back(i);

        ady[j-1] = tempVector_dos;

      }
    }

  }

  return(ady);

}

