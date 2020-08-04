"""
Spyder Editor
"""
import numpy as np
#graphing
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE


def load_dataset(x):
    return np.loadtxt(x)
   # return np.loadtxt(x,delimiter=',')
def euclidian(a,b):
    return np.linalg.norm(a-b)


def kMeans(k, max_dist=0):
    history_centroids = []
    #find distance
    
    dist = euclidian
    data = load_dataset('iris_new_data.txt')
    #data = load_dataset('img_test.txt')
    tsne = TSNE(n_components=2, init='random', random_state=0)
    dataset = tsne.fit_transform(data)
    # dataset = dataset[:, 0:dataset.shape[1] - 1]
    num_instances, num_features = dataset.shape
    #initiate centroid
    centroids = dataset[np.random.randint(0, num_instances - 1, size=k)]
    history_centroids.append(centroids)
    centroids_old = np.zeros(centroids.shape)
    clusters = np.zeros((num_instances, 1))
    norm = dist(centroids, centroids_old)
    iteration = 0
    while norm > max_dist:
        iteration += 1
        norm = dist(centroids, centroids_old)
        centroids_old = centroids
        for index_instance, instance in enumerate(dataset):
            dist_vec = np.zeros((k, 1))
            for index_centroid, centroid in enumerate(centroids):
                dist_vec[index_centroid] = dist(centroid,instance)

            clusters[index_instance, 0] = np.argmin(dist_vec)
            

            print(iteration)
        tmp_centroids = np.zeros((k, num_features))

        for index in range(len(centroids)):
            instances_close = [i for i in range(len(clusters)) if clusters[i] == index]
            centroid = np.mean(dataset[instances_close], axis=0)
            # centroid = dataset[np.random.randint(0, num_instances, size=1)[0]]
            tmp_centroids[index, :] = centroid

        centroids = tmp_centroids

        history_centroids.append(tmp_centroids)
    return centroids, history_centroids, clusters
'''
def bisectingKMeans(dataSet,K,numIterations):
    m,n=shape(dataSet)
    clusterInformation=mat(zeros((m,2)))
    centroidList=[]
    minSSE=inf
    
    #At the first place, regard the whole dataset as a cluster and find the best clusters
    for i in range(numIterations):
        centroid,old_c,clusterAssment=kMeans(3)
        SSE=sum(clusterAssment,axis=0)[0,1]
        if SSE<minSSE:
            minSSE=SSE
            tempCentroid=centroid
            tempCluster=clusterAssment
    centroidList.append(tempCentroid[0].tolist()[0])
    centroidList.append(tempCentroid[1].tolist()[0])
    clusterInformation=tempCluster
    minSSE=inf 
    
    while len(centroidList)<K:
        maxIndex=-2
        maxSSE=-1
        #Choose the cluster with Maximum SSE to split
        for j in range(len(centroidList)):
            SSE=sum(clusterInformation[nonzero(clusterInformation[:,0]==j)[0]])
            if SSE>maxSSE:
                maxIndex=j
                maxSSE=SSE
                
        minIndex=-2
        #Choose the clusters with minimum total SSE to store into the centroidList
        for k in range(numIterations):
            pointsInCluster=dataSet[nonzero(clusterInformation[:,0]==maxIndex)[0]]
            centroid,clusterAssment=kMeans(pointsInCluster, 2)
            SSE=sum(clusterAssment[:,1],axis=0)
            if SSE<minSSE:
                minSSE=SSE
                tempCentroid=centroid.copy()
                tempCluster=clusterAssment.copy()
        #Update the index
        tempCluster[nonzero(tempCluster[:,0]==1)[0],0]=len(centroidList)
        tempCluster[nonzero(tempCluster[:,0]==0)[0],0]=maxIndex
        
        #update the information of index and SSE
        clusterInformation[nonzero(clusterInformation[:,0]==maxIndex)[0],:]=tempCluster
        #update the centrolist
        centroidList[maxIndex]=tempCentroid[0].tolist()[0]
        centroidList.append(tempCentroid[1].tolist()[0])
    return centroid,clusters    
'''      
def fit():
       centroids, history_centroids, clusters = kMeans(3)
       ts= (clusters)
       np.savetxt('results.txt', ts, delimiter='; ')



fit()
