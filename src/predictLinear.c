#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "linear.h"

#define Malloc(type,n) (type *)Calloc(n,type)

extern void print_null(const char *s);

struct feature_node *x;

void do_predict(double *Y, double *X, int *decisionValues, double *DecisionValues, int *proba, double *Probabilities, 
                int *nbSamples, int *sparse, int *rowindex, int *colindex, struct model* model_)
{
  //Rprintf("do_predict - nCl=%i, W=(%f, %f, %f, %f)\n",model_->nr_class,model_->w[0],model_->w[1],model_->w[2],model_->w[3]);
  int nr_feature=get_nr_feature(model_);
  int i, j, n;
  double predict_label;
  double *prob_estimates=NULL;
  double *decision_values=NULL;
  
  if(model_->bias>=0)
    n=nr_feature+1;
  else
    n=nr_feature;
  
  if(*proba)
  {
    if(!check_probability_model(model_))
    {
      Rprintf("ERROR: probability output is only supported for logistic regression.\n");
      return;
    }
    prob_estimates = Malloc(double,model_->nr_class);
  }
  
  if(*decisionValues)
  {
    if(check_regression_model(model_))
    {
      Rprintf("ERROR: decision values output is not supported for regression.\n");
      return;
    }
    decision_values = Malloc(double,model_->nr_class);
  }
  
  //Rprintf("do_predict - ready\n");
  int totalK = 0;
  // PREDICTION PROCESS	
  for(i=0; i<*nbSamples; i++){
    if(*sparse > 0){
      // trying to handle the sparse matrix
      int nnz = rowindex[i+1]-rowindex[i];
      for(j = 0; j<nnz; j++){
        x[j].value = X[totalK];
        x[j].index = colindex[totalK];
        totalK++;
      }
    } else {
      int k = 0;
      for(j=0; j<nr_feature; j++){
        if(X[(nr_feature*i)+j]!=0){
          x[k].value = X[(nr_feature*i)+j];
          x[k].index = j+1;
          k++;
        }
      }
      j = k;
    }
    
    if(model_->bias>=0){
      x[j].index = n;
      x[j].value = model_->bias;
      j++;
    }
    x[j].index = -1;
    
    //Rprintf("do_predict - loop - ready\n");
    if(*proba)
    {
      //Rprintf("do_predict - loop - proba\n");
      predict_label = predict_probability(model_,x,prob_estimates);
      for(j=0;j<model_->nr_class;j++)
        Probabilities[model_->nr_class*i+j]=prob_estimates[j];
    }
    else
    {
      //Rprintf("do_predict - loop - pred\n");
      //Rprintf("model.w - 0x%08x\n",model_->w);
      //Rprintf("x - 0x%08x\n",x);
      predict_label = predict(model_,x);
    }
    //Rprintf("do_predict - loop - computed: %f (%f, %f, %f)\n",predict_label,x[0],x[1],x[2]);
    Y[i]=predict_label;
    
    if(*decisionValues){
      predict_label = predict_values(model_,x,decision_values);
      for(j=0;j<model_->nr_class;j++)
        DecisionValues[model_->nr_class*i+j]=decision_values[j];
    }
  }
  //Rprintf("do_predict - done\n");
  if(*proba)
    Free(prob_estimates);
  if(*decisionValues)
    Free(decision_values);
}

/**
 * Function: predictLinear
 *
 * Author: Thibault Helleputte
 *
 */
void predictLinear(double *Y, double *X, double *W, int *decisionValues, double *DecisionValues, int *proba, double *Probabilities, 
                   int *nbClass, int *nbDim, int *nbSamples, int *sparse, int *rowindex, int *colindex, double *bias, int *labels, int *type){
  
  int n;
  struct model *model_;
  
  set_print_string_function(&print_null);
  
  model_ = load_model(W, nbClass, nbDim, bias, labels, type);
  
  /*	Rprintf("model.W - 0x%08x\n",model_->w);*/
  /*	Rprintf("model.labels - 0x%08x\n",model_->label);*/
  /*	Rprintf("predictLinear - model loaded\n");*/
  if(model_->bias>=0)
    n=model_->nr_feature+1;
  else
    n=model_->nr_feature;
  
  x = Malloc(struct feature_node,n+1);
  /*	Rprintf("x - 0x%08x\n",x);*/
  
  
  do_predict(Y, X, decisionValues, DecisionValues, proba, Probabilities, 
             nbSamples, sparse, rowindex, colindex, model_);
  /*	Rprintf("predictLinear - done predict\n");*/
  // Because a shallow copy was done for W and labels, no need to free them, freeing model_ is enough.
  //free_and_destroy_model(&model_);
  Free(model_);
  Free(x);
  return;
}