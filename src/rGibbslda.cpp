/*
 * Original code by Xuan-Hieu Phan modified by Bettina Gruen
 *
 * Copyright (C) 2007 by
 * 
 * 	Xuan-Hieu Phan
 *	hieuxuan@ecei.tohoku.ac.jp or pxhieu@gmail.com
 * 	Graduate School of Information Sciences
 * 	Tohoku University
 *
 * Copyright (C) 2009 by
 * 
 * 	Bettina Gruen
 *
 * GibbsLDA++ is a free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * GibbsLDA++ is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GibbsLDA++; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */
#include "model.h"

model* lda(int *i, int *j, int *v, int total, 
	  int niters, int verbose, int save, int keep, int estimate_phi, int model_status, int seeded,
	  int K, int M, int V, double alpha, double *beta,
	  string dir, double *init_phi, int *z) 
{
  model* lda = new model();
  lda->niters = niters;
  lda->verbose = verbose;
  lda->save = save;
  lda->keep = keep;
  lda->estimate_phi = estimate_phi;
  lda->seeded = seeded;
  lda->K = K;
  lda->M = M;
  lda->V = V;
  lda->alpha = alpha;
  lda->dir = dir;
  lda->init(i, j, v, total, beta, z, init_phi, model_status);
  lda->estimate();
  lda->inference();
  return(lda);
}

#include <R.h>
#include <Rdefines.h>

extern "C" {

static SEXP returnObjectGibbsLDA(SEXP ans, model * model) {
  SEXP tp, I, J, V, wordassign, nms, z;
  int total, i, j, d;
  int *It, *Jt, *word_new;
  double *m, *Vt;

  tp = PROTECT(allocVector(INTSXP, 1));
  *INTEGER(tp) = model->niters;
  SET_SLOT(ans, install("iter"), tp);
  UNPROTECT(1);

  tp = PROTECT(allocVector(INTSXP, 1));
  *INTEGER(tp) = model->K;
  SET_SLOT(ans, install("k"), tp);
  UNPROTECT(1);

  tp = PROTECT(allocVector(REALSXP, 1));
  REAL(tp)[0] = model->alpha;
  SET_SLOT(ans, install("alpha"), tp);
  UNPROTECT(1);
  
  tp = PROTECT(allocVector(INTSXP, 2));
  INTEGER(tp)[0] = model->M;
  INTEGER(tp)[1] = model->V;
  SET_SLOT(ans, install("Dim"), tp);
  UNPROTECT(1);
  
  tp = PROTECT(allocMatrix(REALSXP, model->K, model->V));
  for (i = 0; i < model->K; i++)
    for (j = 0; j < model->V; j++)
      REAL(tp)[i + model->K * j] = log(model->phi[i][j]);
  SET_SLOT(ans, install("beta"), tp);
  UNPROTECT(1);

  tp = PROTECT(allocMatrix(REALSXP, model->M, model->K));
  m = REAL(tp);
  for (i = 0; i < model->M; i++)
    for (j = 0; j < model->K; j++)
      m[i + model->M * j] = model->theta[i][j];
  SET_SLOT(ans, install("gamma"), tp);
  UNPROTECT(1);
  
  tp = PROTECT(allocVector(REALSXP, 1));
  *REAL(tp) = model->loglikelihood;
  SET_SLOT(ans, install("loglikelihood"), tp);
  UNPROTECT(1);

  if (model->keep > 0) {
    int keepiter = ceil((double)(model->niters/model->keep));
    tp = PROTECT(allocVector(REALSXP, keepiter));
    for (i = 0; i < keepiter; i++) 
      REAL(tp)[i] = model->logLiks[i];
    SET_SLOT(ans, install("logLiks"), tp);
    UNPROTECT(1);
  }

  wordassign = PROTECT(allocVector(VECSXP, 5));
  total = 0;
  for (d = 0; d < model->M; d++) {
    total += model->ptrndata->docs[d]->length;
  }
  It = (int*)malloc(sizeof(int) * total);
  Jt = (int*)malloc(sizeof(int) * total);
  Vt = (double*)malloc(sizeof(double) * total);
  
  i = 0;
  for (d = 0; d < model->M; d++) {    
    word_new = (int*)malloc(sizeof(int) * (model->V));
    for (j = 0; j < model->V; j++) {
      word_new[j] = 0;
    }
    for (j = 0; j < model->ptrndata->docs[d]->length; j++) {
      if (word_new[model->ptrndata->docs[d]->words[j]] != 1) {
	It[i] = d + 1;
	Jt[i] = model->ptrndata->docs[d]->words[j] + 1;
	Vt[i] = model->wordassign[d][j] + 1;
	i++;
      }
      word_new[model->ptrndata->docs[d]->words[j]] = 1;
    }
    free(word_new);
  }
  I = PROTECT(allocVector(INTSXP, i));
  J = PROTECT(allocVector(INTSXP, i));
  V = PROTECT(allocVector(REALSXP, i));
  for (j = 0; j < i; j++) {    
      INTEGER(I)[j] = It[j];
      INTEGER(J)[j] = Jt[j];
      REAL(V)[j] = Vt[j];
  }
  SET_VECTOR_ELT(wordassign, 0, I); 
  SET_VECTOR_ELT(wordassign, 1, J);
  SET_VECTOR_ELT(wordassign, 2, V);
  UNPROTECT(3);

  tp = PROTECT(allocVector(INTSXP, 1));
  INTEGER(tp)[0] = model->M;
  SET_VECTOR_ELT(wordassign, 3, tp);
  UNPROTECT(1);
  tp = PROTECT(allocVector(INTSXP, 1));
  INTEGER(tp)[0] = model->V;
  SET_VECTOR_ELT(wordassign, 4, tp);
  UNPROTECT(1);

  setAttrib(wordassign, R_NamesSymbol, nms = allocVector(STRSXP, 5));
  SET_STRING_ELT(nms, 0, mkChar("i"));
  SET_STRING_ELT(nms, 1, mkChar("j"));
  SET_STRING_ELT(nms, 2, mkChar("v"));
  SET_STRING_ELT(nms, 3, mkChar("nrow"));
  SET_STRING_ELT(nms, 4, mkChar("ncol"));
  setAttrib(wordassign, R_ClassSymbol, mkString("simple_triplet_matrix"));
  SET_SLOT(ans, install("wordassignments"), wordassign);
  UNPROTECT(1);
  free(It); free(Jt); free(Vt);

  z = PROTECT(allocVector(INTSXP, total));
  i = 0;
  for (d = 0; d < model->M; d++) {    
    for (j = 0; j < model->ptrndata->docs[d]->length; j++) {
      INTEGER(z)[i] = model->z[d][j] + 1;
      i++;
    }
  }
  SET_SLOT(ans, install("z"), z);
  UNPROTECT(1);
  return(ans);
}

SEXP rGibbslda(SEXP i, SEXP j, SEXP v, SEXP nrow, SEXP ncol,
	       SEXP control, SEXP initialize, SEXP seeded, SEXP seed, 
	       SEXP k, SEXP prefix, SEXP phi, SEXP z) 
{
  SEXP ans, cls;
  double *init_phi, *delta;
  int *init_z;

  init_z = NULL; 
  init_phi = NULL;

  if (*INTEGER(initialize) == 1 || *INTEGER(GET_SLOT(control, install("estimate.beta"))) == 0) {
    init_phi = REAL(phi);
  } 
  if (*INTEGER(initialize) == 2) {
    init_z = INTEGER(z);
  }
  if (*LOGICAL(seeded) == 1){
    delta = REAL(seed);
  } else {
    delta = REAL(GET_SLOT(control, install("delta")));
  }

  GetRNGstate();
  model* model = lda(INTEGER(i),
		    INTEGER(j),
		    INTEGER(v),
		    LENGTH(v),
		    *INTEGER(GET_SLOT(control, install("iter"))),
		    *INTEGER(GET_SLOT(control, install("verbose"))),
		    *INTEGER(GET_SLOT(control, install("save"))),
		    *INTEGER(GET_SLOT(control, install("keep"))),
		    *LOGICAL(GET_SLOT(control, install("estimate.beta"))),
		    *INTEGER(initialize),
		    *LOGICAL(seeded),
		    *INTEGER(k),
		    *INTEGER(nrow), 
		    *INTEGER(ncol),
		    *REAL(GET_SLOT(control, install("alpha"))),
		    delta,
		    CHAR(asChar(prefix)),
		    init_phi,
		    init_z);
  // construct return object
  PROTECT(cls = MAKE_CLASS("LDA_Gibbs"));
  PROTECT(ans = NEW_OBJECT(cls));
  ans = returnObjectGibbsLDA(ans, model);
  PutRNGstate();
  UNPROTECT(2);
  delete model;
  return(ans);
}
}
