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

#ifndef	_DATASET_H
#define	_DATASET_H

#include <string>
#include <vector>
#include <map>

using namespace std;

class document {
public:
    int * words;
    int length;
    
    document() {
	words = NULL;
	length = 0;	
    }
    
    document(int nlength) {
	this->length = nlength;
	this->words = new int[nlength];	
    }
    
    document(int nlength, int * nwords) {
	this->length = nlength;
	this->words = new int[nlength];
	for (int i = 0; i < nlength; i++) {
	    this->words[i] = nwords[i];
	}
    }

    document(vector<int> & doc) {
	this->length = doc.size();
	this->words = new int[this->length];
	for (int i = 0; i < this->length; i++) {
	    this->words[i] = doc[i];
	}
    }

    ~document() {
	if (words) {
	    delete[] words;
	}
    }
};

class dataset {
public:
    document ** docs;
    int M; // number of documents
    int V; // number of words
    
    dataset() {
	docs = NULL;
	M = 0;
	V = 0;
    }
    
    dataset(int nM, int nV) {
	this->M = nM;
	this->V = nV;
	docs = new document*[nM];	
    }   
    
    ~dataset() {
	if (docs) {
	    for (int i = 0; i < M; i++) {
	      delete docs[i];
	    }
	    delete[] docs;
	}
    }
    
    void add_doc(document * doc, int idx) {
	if (0 <= idx && idx < M) {
	    docs[idx] = doc;
	}
    }   

    int readDocumentTermMatrix(int *i, int *j, int *v, int total);
};

#endif

