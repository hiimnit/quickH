#include <iostream>
#include <iomanip>
#include <vector>
#include <fstream>
#include <algorithm>
#include <omp.h>
#include <cmath>

using namespace std;

struct Point {
public:
    Point() : x(0), y(0) {}
    Point(float x, float y) : x(x), y(y) {}
    float x;
    float y;
    friend ostream& operator<<(ostream& stream, const Point& point) {
        stream << setprecision(16) << point.x << " " << point.y;
        return stream;
    }
};


class NormalFormLine {
    // p : a * x + b * y + c = 0
    // A[a1, a2]
    // v(p, A) = abs(a * a1 + b * a2 + c) / sqrt(a * a + b * b)

    // 1 / sqrt(a * a + b * b) - used a ton
    //  => invsqrt = 1 / sqrt(a * a + b * b)
    double a, b, c;
    double invsqrt; // invsqrt = 1 / sqrt(a * a + b * b) netreba
public:
    NormalFormLine(Point *A, Point *B) {
        // normal vector = vector + swap
        a = (B->y - A->y) * -1;
        b = B->x - A->x;
        c = -a * A->x - b * A->y;
        invsqrt = 1 / sqrt(a * a + b * b); // useless ?
    }
    int compare(Point * P) {
        double res = a * P->x + b * P->y + c;
        if (res > 0) {
            return 1;
        }
        return -1;
    }
    double getDistance(Point * P) {
        double t = a * P->x + b * P->y + c;
        if (t < 0) t *= -1; // abs()
        return t * invsqrt; // je treba prenasobit vse stejnou kladnou hodnotou ? -> neni
    }
};

Point ** getPoints(string file, int * size) {
    Point ** points;

    if(file == "test") {
        *size = 18;
        points = new Point*[*size];
        points[0] = new Point(-5, 0);
        points[1] = new Point(0, -5);
        points[2] = new Point(5, 0);
        points[3] = new Point(0, 5);
        points[4] = new Point(-4, 3);
        points[5] = new Point(-4, -3);
        points[6] = new Point(4, 3);
        points[7] = new Point(4, -3);
        points[8] = new Point(3, 4);
        points[9] = new Point(3, -4);
        points[10] = new Point(-3, 4);
        points[11] = new Point(-3, -4);
        points[12] = new Point(1, 1);
        points[13] = new Point(-1, 1);
        points[14] = new Point(1, -1);
        points[15] = new Point(-1, -1);
        points[16] = new Point(0, 0);
        points[17] = new Point(4, 4);
        return points;
    }

    fstream fs(file, fstream::in);
    char line[256];
    int number;
    float x, y;

    if(!fs.is_open()) {
        cout << "Failed to open " << file << endl;
        return NULL;
    }

    fs.getline(line, 256);
    fs >> number;

    *size = number;
    points = new Point*[number];

    while(number-- > 0) {
        fs >> x;
        fs >> y;
        points[number] = new Point(x, y);
    }

    fs.close();
    return points;
}

bool cmp(const Point * left, const Point * right) {
    if (left->x == right->x) return left->y < right->y;
    return left->x < right->x;
}

bool checkPoints(string file, Point ** myResult, int size) {
    int number;
    float x, y;
    fstream fs(file, fstream::in);
    if(!fs.is_open()) {
        cout << "Failed to open " << file << endl;
        return false;
    }

    fs >> number; // reading dimension number
    fs >> number; // reading points number

    if(number != size) {
        fs.close();
        return false;
    }

    Point ** res = new Point*[number];
    while(number-- > 0) {
        fs >> x;
        fs >> y;
        res[number] = new Point(x, y);
    }

    sort(myResult, myResult + size, cmp);
    sort(res, res + size, cmp);

    for(int i = 0; i < size; ++i) {
        if (myResult[i]->x != res[i]->x || myResult[i]->y != res[i]->y) {
            fs.close();
            for(int i = 0; i < number; ++i) delete res[i];
            delete res;
            return false;
        }
    }

    fs.close();
    for(int i = 0; i < number; ++i) delete res[i];
    delete res;
    return true;
}

void findHull(Point * left, Point * right, Point ** points, int size, NormalFormLine * nfl, int dir, Point ** result, int * res_size) {
    if (left->x == right->x && left->y == right->y) return;         // mozna osetrit jeste v quickHull()
    double f = 0, temp;

    Point * furthest = NULL;
    for(int i = 0; i < size; ++i) {
        temp = nfl->getDistance(points[i]);
        if(temp > f) {
            f = temp;
            furthest = points[i];
        }
    }

    if (furthest == NULL) {
        result[*res_size] = right;
        *res_size += 1;
        return;
    }

    NormalFormLine nflL(left, furthest);
    NormalFormLine nflR(furthest, right);

    Point ** L = new Point*[size];
    Point ** R = new Point*[size];

    int k, s1 = 0, s2 = 0;
    for(int i = 0; i < size; ++i) {
        k = nflL.compare(points[i]);
        if(k > 0) {
            L[s1++] = points[i];
            continue;
        }
        k = nflR.compare(points[i]);
        if(k > 0) {
            R[s2++] = points[i];
        }
    }

    findHull(left, furthest, L, s1, &nflL, dir, result, res_size);
    delete[] L;
    findHull(furthest, right, R, s2, &nflR, dir, result, res_size);
    delete[] R;
}

Point ** quickHull(Point ** points, int size, int * res_size) {
    // osetreni male size ?

    Point * leftmost = points[0], * rightmost = points[0], * topmost = points[0], * bottommost = points[0];

    // vynechavam moznost se vsesmi body v jedne line
    for (int i = 0; i < size; ++i) {
        if (points[i]->x < leftmost->x) {
            leftmost = points[i];
        } else if (points[i]->x > rightmost->x) {
            rightmost = points[i];
        }

        if (points[i]->y < bottommost->y) {
            bottommost = points[i];
        } else if (points[i]->y > topmost->y) {
            topmost = points[i];
        }
    }

    Point ** result = new Point*[size];

    Point ** LT = new Point*[size];
    Point ** TR = new Point*[size];
    Point ** RB = new Point*[size];
    Point ** BL = new Point*[size];

    int s1 = 0, s2 = 0, s3 = 0, s4 = 0;

    NormalFormLine nflLT(leftmost, topmost);
    NormalFormLine nflTR(topmost, rightmost);
    NormalFormLine nflRB(rightmost, bottommost);
    NormalFormLine nflBL(bottommost, leftmost);

    int k = 0;
    for(int i = 0; i < size; ++i) {
        k = nflLT.compare(points[i]);
        if (k > 0) {
            LT[s1++] = points[i];
            continue;
        }
        k = nflTR.compare(points[i]);
        if (k > 0) {
            TR[s2++] = points[i];
            continue;
        }
        k = nflRB.compare(points[i]);
        if (k > 0) {
            RB[s3++] = points[i];
            continue;
        }
        k = nflBL.compare(points[i]);
        if (k > 0) {
            BL[s4++] = points[i];
        }
    }


    // nfl nebudu posilat ?
    *res_size = 0;
    findHull(leftmost, topmost, LT, s1, &nflLT, 0, result, res_size);
    delete[] LT;
    findHull(topmost, rightmost, TR, s2, &nflTR, 0, result, res_size);
    delete[] TR;
    findHull(rightmost, bottommost, RB, s3, &nflRB, 1, result, res_size);
    delete[] RB;
    findHull(bottommost, leftmost, BL, s4, &nflBL, 1, result, res_size);
    delete[] BL;

    return result;
}

int main (int argc, char* argv[]) {
    if(argc != 3) {
        cout << "usage : ./qh.o infile resfile" << endl;
        return 1;
    }
    string in_file  = argv[1];
    string res_file = argv[2];
    double start_time, end_time;

    cout << "              in_file : " << in_file  << endl;
    cout << "             res_file : " << res_file << endl;

    int size, res_size;
    Point ** points = getPoints(in_file, &size);

    start_time = omp_get_wtime();
    cout << "          Points read : " << size << endl;

    Point ** result = quickHull(points, size, &res_size);
    cout << "Points in convex hull : " << res_size << endl;

    end_time = omp_get_wtime();

    cout << "                 Time : " << end_time - start_time << "s" << endl;

    if (res_file != "0") {
        cout << "              results : ";
        if (checkPoints(res_file, result, res_size)) {
            cout << "correct" << endl;
        } else {
            cout << "incorrect" << endl;
        }
    }

    // todo : cleanup

    return 0;
}