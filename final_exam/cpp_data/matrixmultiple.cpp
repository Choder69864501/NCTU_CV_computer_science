#include<iostream>
#define endl '\n'
using namespace std;
int a,b,m;
struct matrix{
	long long int v[2][2];
	matrix(){}
	matrix(long long int a,long long int b,long long int c,long long int d){
		v[0][0]=a;
		v[0][1]=b;
		v[1][0]=c;
		v[1][1]=d;
	}
	matrix operator*(const matrix&rhs)const{
		matrix res;
		for(int i=0;i<2;i++)
			for(int j=0;j<2;j++){
				res.v[i][j]=0;
				for(int k=0;k<2;k++){
					res.v[i][j]=((this->v[i][k]*rhs.v[k][j])%m+(res.v[i][j]%m))%m;
				}
			}
		return res;
	}
};
matrix pow(matrix tran,int j){
	matrix res(1,0,0,1);
	for(;j;j>>=1){
		if(j&1)res=res*tran;
		tran=tran*tran;
	}
	return res;
}
int main(int argc,char*argv[]){
	freopen(argv[1],"r",stdin);
	freopen("output_cpp.txt","w",stdout);
	matrix tran=(matrix){1,1,1,0},res;
	int k;
	while(cin>>a>>b>>k>>m){
		if(k==1){
			cout<<a<<endl;
			continue;
		}
		if(k==2){
			cout<<b<<endl;
			continue;
		}
		res=pow(tran,k-2);
		cout<<(res.v[0][0]%m*b+res.v[0][1]%m*a)%m<<endl;
	}
	return 0;
}
