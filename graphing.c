#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <math.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

int width = 900 ;
int height = 600 ;

int offset = 25 ;
int wside = 15 ;

int map_size = 80 ;

int min(int x, int y) {
    if(x < y) {
        return x;
    }
    return y ;
}

int max(int x, int y) {
    if(x > y) {
        return x;
    }
    return y ;
}

int int_of_string(char* s) {
    int i = 0;
    int res = 0 ;
    while(s[i] != '\0' && (int)(s[i]) >= 48 && (int)(s[i]) <= 57) {
        res = res*10 + (int)(s[i]) - 48 ;
        i += 1;
    }
    return res ;
}

int truncated_float_of_string(char* s) {
    int i = 0;
    int res = 0 ;
    while(s[i] != '\0' && s[i] != '.') {
        res = res*10 + (int)(s[i]) - 48 ;
        i += 1;
    }
    return res ;
}

char* input_line(FILE* ptr) {
    char* res = malloc(sizeof(char)*256) ;
    int i = 0 ;
    char c = fgetc(ptr) ;
    while(c != EOF && c != '\n') {
        res[i] = c;
        i += 1;
        c = fgetc(ptr) ;
    }
    res[i] = '\0' ;
    return res;
}

void flush_line(FILE* ptr) {
    char c = fgetc(ptr) ;
    while(c != EOF && c != '\n') {
        c = fgetc(ptr);
    }
}

int input_int(FILE* ptr) {
    int res = 0 ;
    char c = '0' ;
    while(c != EOF && (int)c >= 48 && (int)c <= 57) {
        res = 10*res + (int)c - 48 ;
        c = fgetc(ptr);
    }
    return res ;
}

void updateRenderer(SDL_Renderer* renderer) {
    SDL_RenderPresent(renderer);
}

void setRendererColor(SDL_Renderer* renderer, int r, int g, int b, int a) {
    SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

void resetRenderer(SDL_Renderer* renderer) {
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
}

void refreshRenderer(SDL_Renderer* renderer) {
    resetRenderer(renderer);
    updateRenderer(renderer);
}

void drawRectToRenderer(SDL_Renderer* renderer, SDL_Rect* rect, int R, int G, int B, int A) {
    SDL_SetRenderDrawColor(renderer, R, G, B, A);
    SDL_RenderFillRect(renderer, rect);
}

void placeRectToRenderer(SDL_Renderer* renderer, int X, int Y, int W, int H, int R, int G, int B, int A) {
    SDL_Rect rect;
    rect.x = X;
    rect.y = Y;
    rect.w = W;
    rect.h = H;
    SDL_SetRenderDrawColor(renderer, R, G, B, A);
    SDL_RenderFillRect(renderer, &rect);
}

void placeRectToRendererNoColor(SDL_Renderer* renderer, int X, int Y, int W, int H) {
    SDL_Rect rect;
    rect.x = X;
    rect.y = Y;
    rect.w = W;
    rect.h = H;
    SDL_RenderFillRect(renderer, &rect);
}

int ln_b(int b, int n) {
    if(n < 0) {
        return ln_b(b, -n) ;
    } else if(n < b) {
        return 0;
    }
    return 1 + ln_b(b, n/b) ;
}

void drawDigitToRenderer(SDL_Renderer* renderer, int d, int width, int X, int Y, int W, int H) {
    switch (d) {
        case 0:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, 2*width, H+2*width); // 5
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 1:
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            break;
        case 2:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, 2*width, H+2*width); // 5
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 3:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 4:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            break;
        case 5:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 6:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, 2*width, H+2*width); // 5
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 7:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            break;
        case 8:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, 2*width, H+2*width); // 5
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        case 9:
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, W+2*width, 2*width); // 1
            placeRectToRendererNoColor(renderer, X-width/2, Y-width/2, 2*width, H+2*width); // 2
            placeRectToRendererNoColor(renderer, X+W-width/2, Y-width/2, 2*width, H+2*width); // 3
            placeRectToRendererNoColor(renderer, X-width/2, Y+H-width/2, W+2*width, 2*width); // 4
            placeRectToRendererNoColor(renderer, X+W-width/2, Y+H-width/2, 2*width, H+2*width); // 6
            placeRectToRendererNoColor(renderer, X-width/2, Y+2*H-width/2, W+2*width, 2*width); // 7
            break;
        default:
            break;
    }
}

void drawIntegerToRenderer(SDL_Renderer* renderer, int n, int width, int X, int Y, int W, int H, int R, int G, int B, int A) {
    setRendererColor(renderer, R, G, B, A) ;
    int dsize = 1+ln_b(10, n) ;
    int cur_n = n ;
    for(int i = 0; i < dsize; i++) {
        drawDigitToRenderer(renderer, cur_n%10, width, X+(W+3*width)*(dsize-1-i), Y, W, H) ;
        cur_n = cur_n / 10 ;
    }
}

void drawCircleToRenderer(SDL_Renderer * renderer, int32_t centreX, int32_t centreY, int32_t radius) {
   const int32_t diameter = (radius * 2);

   int32_t x = (radius - 1);
   int32_t y = 0;
   int32_t tx = 1;
   int32_t ty = 1;
   int32_t error = (tx - diameter);

   while (x >= y)
   {
      // each of the following renders an octant of the circle
      SDL_RenderDrawPoint(renderer, centreX + x, centreY - y);
      SDL_RenderDrawPoint(renderer, centreX + x, centreY + y);
      SDL_RenderDrawPoint(renderer, centreX - x, centreY - y);
      SDL_RenderDrawPoint(renderer, centreX - x, centreY + y);
      SDL_RenderDrawPoint(renderer, centreX + y, centreY - x);
      SDL_RenderDrawPoint(renderer, centreX + y, centreY + x);
      SDL_RenderDrawPoint(renderer, centreX - y, centreY - x);
      SDL_RenderDrawPoint(renderer, centreX - y, centreY + x);

      if (error <= 0)
      {
         ++y;
         error += ty;
         ty += 2;
      }

      if (error > 0)
      {
         --x;
         tx += 2;
         error += (tx - diameter);
      }
   }
}

int get_data(char* filename, int** board, bool* ended) {
    // if board has been modified //
    FILE* ptr = fopen(filename, "r") ;

    int cur_t = input_int(ptr) ;
    char ignored = '0' ;

    flush_line(ptr);
    flush_line(ptr);

    int maze_w = input_int(ptr) ;
    int maze_h = input_int(ptr) ;

    board[cur_t%map_size][0] = 0 ;
    board[cur_t%map_size][1] = 0 ;
    board[cur_t%map_size][2] = 0 ;
    board[cur_t%map_size][3] = 0 ;

    for(int w = 0; w < maze_w; w++) {
        for(int h = 0; h < maze_h; h++) {
            int tile = input_int(ptr) ;
            //fprintf(stderr, "%d ", tile) ;
            if(tile >= 3) {
                board[cur_t%map_size][tile-3] += 1;
            }
        }
        ignored = fgetc(ptr); // \n
        //fprintf(stderr, "\n") ;
    }

    int nbombs = input_int(ptr) ;
    for(int k = 0; k < nbombs; k++) {
        flush_line(ptr);
    }

    int nplayers = input_int(ptr) ;
    if(nplayers == 1) {
        *ended = true ;
    }

    //fprintf(stderr, "(%d, %d, %d, %d, %d)\n", cur_t, maze_w, maze_h, nbombs, nplayers);

    fclose(ptr) ;
    return cur_t ;
}

int convex_pt(int a, int b, float theta) {
    return (a + (int)((b - a) * theta)) ;
}

void playerActions() {
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                break;
            case SDL_KEYDOWN:
                switch (event.key.keysym.sym) {
                    case SDLK_t:
                        exit(1);
                        break;
                }
        }
    }
}

void update_graph(SDL_Renderer* renderer, int** board, int current_time) {
    resetRenderer(renderer) ;
    placeRectToRenderer(renderer, offset, offset, width - 2*offset, height - 2*offset, 128, 128, 128, 255) ;
    placeRectToRenderer(renderer, offset + wside, offset + wside, width - 2*(offset + wside), height - 2*(offset + wside), 255, 255, 255, 255) ;

    playerActions();

    int max_val = 0 ;
    for(int k = 0; k < map_size; k++) {
        for(int l = 0; l < 4; l++) {
            max_val = max(max_val, board[k][l]) ;
        }
    }

    int left_x = offset + wside ;
    int bot_y = offset + wside ;
    int right_x = width - 2*(offset + wside) ;
    int top_y = height - 2*(offset + wside) ;

    int borned = min(map_size, current_time) ;

    int x_size = 1+(right_x - left_x)/borned ;
    int y_size = 1+(top_y - bot_y)/(1+max_val) ;

    for(int t0 = 0; t0 < borned; t0++) {
        int t = (t0 + max(0, current_time - map_size +1))%map_size ;
        int xc = convex_pt(left_x, right_x, ((float)(t0))/((float)(borned))) ;
        if(t%20==0) {
            placeRectToRenderer(renderer, xc, offset+wside, x_size, height - 2*(offset+wside), 128, 128, 128, 128);
        }
        for(int pl = 0; pl < 4; pl++) {
            //int t = t0 ;
            int yc = convex_pt(bot_y, top_y, 1.0f - ((float)(max(0, board[t][pl])))/((float)(max_val+1))) ;
            //drawCircleToRenderer(renderer, xc, yc, 10) ;
            placeRectToRenderer(renderer, xc, yc-y_size+offset+wside, x_size, y_size, 255*(pl==0)+192*(pl==3), 255*(pl==2)+192*(pl==3), 255*(pl==1), 96) ;

        }
    }
    int tmax = (borned-1 + max(0, current_time - map_size +1))%map_size ;
    for(int pl = 0; pl < 4; pl++) {
        int mval = max(0, board[tmax][pl]);
        int yc = convex_pt(bot_y, top_y, 1.0f - ((float)(max(0, board[tmax][pl])))/((float)(max_val+1))) ;
        drawIntegerToRenderer(renderer, mval, 2, right_x+2-20*(-1+ln_b(10, mval)), yc+14-2*2, 14, 14, 255*(pl==0)+192*(pl==3), 255*(pl==2)+192*(pl==3), 255*(pl==1), 128) ;
    }

    drawIntegerToRenderer(renderer, current_time, 2, 375, 105/3+3, 75/3, 105/3, 0, 0, 0, SDL_ALPHA_OPAQUE);

    updateRenderer(renderer) ;
}

bool can_pass() {
    FILE* ptr = fopen("signal.txt", "r") ;
    bool cp = false ;
    if(fgetc(ptr) == '1') {
        FILE* pptr = fopen("signal.txt", "w") ;
        fprintf(pptr, "0") ;
        fclose(pptr);
        cp = true ;
    }
    fclose(ptr) ;
    return cp ;
}

int main(int argc, char** argv) {
    fprintf(stderr, "|||||||||||||||| ENTER ||||||||||||||||\n") ;
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        fprintf(stderr, "error initializing SDL: %s\n", SDL_GetError());
    }
    SDL_Window* win = SDL_CreateWindow("statistics because why not :)", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height, 0);
 
    Uint32 render_flags = SDL_RENDERER_ACCELERATED;
    SDL_Renderer* rend = SDL_CreateRenderer(win, -1, render_flags);
    SDL_SetRenderDrawBlendMode(rend, SDL_BLENDMODE_BLEND);

    int** board = malloc(sizeof(int*)*map_size) ;
    for(int k = 0; k < map_size; k++) {
        board[k] = malloc(sizeof(int)*4);
        for(int l = 0; l < 4; l++) {
            board[k][l] = (-1) ;
        }
    }

    bool game_ended = false ;
    int last_time = (-1) ;

    usleep(250000);

    //int cur_time = get_data("entrees.txt", board, &game_ended) ;
    //assert(false);

    while(!game_ended) {
        while(!can_pass()) {
            // required because of non-synchronization causing this to read a non-full file
        }
        int cur_time = get_data("entrees.txt", board, &game_ended) ;
        //fprintf(stderr, "%d %d %d\n", last_time, cur_time, game_ended) ;
        if(cur_time != last_time) {
            update_graph(rend, board, cur_time) ;
            last_time = cur_time ;
        }
    }

    fprintf(stderr, "|||||||||||||||| QUIT ||||||||||||||||\n") ;

    for(int k = 0; k < map_size; k++) {
        free(board[k]);
    }
    free(board);

    SDL_DestroyRenderer(rend);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}