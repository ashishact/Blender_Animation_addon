/**
 * Created by ashishact on 07/06/2015.
 * For Phaser
 */
// gg=0;
window.BLEN2D = {

    assets: {},
    objects: {},

    game: null,
    data: null,

    init: function (game) {
        this.game = game;
        this.animation.Time = game.time;
		this.game.load.crossOrigin = "anonymous";
        // gg=this.data
    },

    setBackgroundColor: function (bg, appendToBody) {

        if (this.game.stage.backgroundColor != bg) {
            this.game.stage.setBackgroundColor(bg);
        }
        if (appendToBody) {
            document.body.style.backgroundColor = bg;
        }
    },

    // preload all assets
    preload: function (stateName) {
        var assets = {}
        if(stateName == 'all'){
            var assetsContents = this.data["@Assets Library"].contents;
            for(var i=0; i < assetsContents.length; i++ ){
                this._preloadState(assetsContents[i], assets); 
            }
        }
        else{
            var assetsContents = this.data["@Assets Library"].contents;
            for(var i=0; i < assetsContents.length; i++ ){
                if(assetsContents[i].name == stateName){
                    this._preloadState(assetsContents[i], assets);
                }
                
            }
        }
        return assets;

    },
    _preloadState: function(obj, assets){
        if(obj.type == 'EMPTY'){

        }
        else if(obj.type=='MESH') {
            assets[obj.name] = this._loadImagesToPhaser(obj.texture, obj.texturepath);
        }
        var children = obj.contents;
        for(var i=0; i < children.length; i++ ){
            this._preloadState(children[i], assets);
        }

    },

    create: function(stateName){
        var objects = {}
        if(stateName == 'all'){
            var objectContents = this.data["@HTML5 root"].contents;
            for(var i=0; i < objectContents.length; i++ ){
                this._loadSpritesToPhaser(null, objectContents[i], objects);
            }
        }
        else{
            var objectContents = this.data["@HTML5 root"].contents;
            for(var i=0; i < objectContents.length; i++ ){
                if(objectContents[i].name == stateName){
                    this._loadSpritesToPhaser(null, objectContents[i], objects);
                }
            }      
            
        }
        return objects
    },


    getGameBoundary: function(){
        var propcs = this.data["@Properties"].contents;
        var gameBoundary = {'w':600, 'h':400};
        for(var i=0; i < propcs.length; i++){
            if(propcs[i].name == 'game boundary'){
                gameBoundary = {'w':propcs[i].gameWidth, 'h':propcs[i].gameHeight};
                return gameBoundary;
            }
        }
        return gameBoundary;
        
    },

    _loadAssets: function(){

    },

    _loadSpritesToPhaser: function(parent, obj, objects){
        var sprite;
        if(obj.type == 'MESH') {
            sprite = this.game.add.sprite(obj.x, obj.y, obj.texture);
            sprite.anchor.setTo(obj.anchorx, obj.anchory);
            sprite.pivot.setTo(obj.anchorx, obj.anchory);
            sprite.scale.setTo(obj.scalex, obj.scaley);
            sprite.rotation = obj.rotation
            sprite.name = obj.name
        }
        else if (obj.type == 'EMPTY'){
            sprite = this.game.add.group(this.game.world, obj.name, false,false,0)
            sprite.position.setTo(obj.x, obj.y)
            sprite.rotation = obj.rotation
            sprite.scale.setTo(obj.scalex, obj.scaley);
        }

        sprite.blen2danimation = {
            currentAnimation: null,
            currentAction: null
        };

        if(obj.animationsCount && obj.actionsCount){
            sprite.blen2danimation = {
                animations:obj.animations,
                actions: obj.actions,
                currentAnimation: null,
                currentAction: null,
                startTime: 0,
                looptime: null,// time in miliseconds to complete this animation once
                loopcount: 0,

                playing: false,
                fps:10,
                loop:false,
                BEG:0,
                END:0

            };
        }
        else{
            if (obj.animationsCount){
                sprite.blen2danimation = {
                    animations:obj.animations,
                    actions: null,
                    currentAnimation: null,
                    currentAction: null,
                    startTime: 0,
                    looptime: null,// time in miliseconds to complete this animation once
                    loopcount: 0,

                    playing: false,
                    fps:10,
                    loop:false,
                    BEG:0,
                    END:0

                };
            }
            if (obj.actionsCount){
                sprite.blen2danimation = {
                    actions: obj.actions,
                    currentAction: null,
                    animations: null,
                    currentAnimation: null

                };
            }
        }
        
        
        if(parent == null){
        }
        else{
            parent.addChild(sprite);
        }

        objects[obj.name] = sprite;
        var objectContents = obj.contents;
        for(var i=0; i < objectContents.length; i++ ){
            this._loadSpritesToPhaser(sprite, objectContents[i], objects);
        }

    },

    _loadImagesToPhaser: function(name, filepath){
        return this.game.load.image(name, filepath);
    },

    getBlend2dAnimation: function(obj){

    },

    animation:{
        Time: null,
        currentAnimationList:[],
        start:function(obj, animationName, fps, loop, BEG, END){
            if (obj.blen2danimation){
                b2danim=obj.blen2danimation;




                if(b2danim.animations.hasOwnProperty(animationName)){
                    b2danim.currentAnimation = b2danim.animations[animationName];


                    b2danim.fps = fps;
                    b2danim.loop = loop;
                    
                    if (BEG==null){
                        b2danim.BEG = b2danim.currentAnimation.minmaxFrame[0];
                    }
                    else{
                        b2danim.BEG = BEG;
                    }

                    if (END==null){
                        b2danim.END = b2danim.currentAnimation.minmaxFrame[1];
                    }
                    else{
                        b2danim.END = END;
                    }
                    b2danim.loopcount = 0;
                    if(fps){
                        b2danim.looptime = Math.round(((b2danim.END - b2danim.BEG)/fps)*1000);
                    }
                    else{
                        b2danim.looptime = 1000;
                    }
                        

                    
                    this.setCurrentActionsToNone(obj);
                    for(var objectName in b2danim.currentAnimation.objactbinding){
                        actionName = b2danim.currentAnimation.objactbinding[objectName];
                        this.setCurrentActions(obj, objectName, actionName, b2danim );
                    }



                    b2danim.playing = true;
                    b2danim.startTime = this.Time.now;

                    obj.blen2danimation = b2danim;
                    this.currentAnimationList.push(obj);

                }
            }
        },
        stop: function(obj){
            for (var i =0; i < currentAnimationList.length; i++){
                if(obj == currentAnimationList[i]){
                    currentAnimationList.splice(i,1);
                }
            }
        },
        play: function(obj, value){
            obj.blen2danimation.playing = value;
        },
        updateActions: function(anim, obj, elaspedTime){
            // console.log(obj, obj.blen2danimation.currentAction);
            if(obj.blen2danimation.currentAction){

                frame_now = anim.BEG + ((elaspedTime%anim.looptime)/anim.looptime)*(anim.END - anim.BEG);

                currkeyframes = obj.blen2danimation.currentAction.keyframes;
                
                elaspedTimeFromBEG = elaspedTime%anim.looptime;

                for(var prop in currkeyframes){
                    if(currkeyframes[prop].fromKeyframeIndex < 0){
                        // console.log(loop_count);
                        fromkeyframe = currkeyframes[prop].datalist[0];
                        v = fromkeyframe.value;
                        // console.log(prop);
                        if(prop == 'x'){
                            obj.position.x = v;
                        }
                        else if(prop == 'y'){
                            obj.position.y = v;
                        }
                        else if(prop == 'rotation'){
                            obj.rotation = v;
                        }
                        else if(prop == 'scalex'){
                            obj.scale.x = v;
                        }
                        else if(prop == 'scaley'){
                            obj.scale.y = v;
                        }

                        if(frame_now >= fromkeyframe.frame){
                            currkeyframes[prop].fromKeyframeIndex = 0;
                        }
                    }
                    else{
                        // console.log(prop);

                        fromkeyframe = currkeyframes[prop].datalist[currkeyframes[prop].fromKeyframeIndex];
                        if(currkeyframes[prop].fromKeyframeIndex < currkeyframes[prop].datalist.length-1){
                            tokeyframe = currkeyframes[prop].datalist[currkeyframes[prop].fromKeyframeIndex + 1];

                            // from BEG
                            // fromkeyframe_time = ((fromkeyframe.frame - anim.BEG)/anim.fps)*1000;
                            // tokeyframe_time = ((tokeyframe.frame - anim.BEG)/anim.fps)*1000;

                            // keyframe_width_time = tokeyframe_time - fromkeyframe_time;
                            // time_now_from_fromkeyframe = elaspedTimeFromBEG - fromkeyframe_time;

                            // if(time_now_from_fromkeyframe >= keyframe_width_time){//change index to nex keyframe
                            //     currkeyframes[prop].fromKeyframeIndex +=1;
                            // }

                            if(keyframe_width_time){
                                // interpolation_fac = time_now_from_fromkeyframe/keyframe_width_time;
                                interpolation_fac = (frame_now - fromkeyframe.frame)/(tokeyframe.frame - fromkeyframe.frame);
                            }
                            else{
                                interpolation_fac = 1;
                            }

                            console.log(inp - interpolation_fac);

                            v1 = fromkeyframe.value;
                            v2 = tokeyframe.value;
                            v = v1 + interpolation_fac*(v2 - v1);
                    
                            if(prop == 'x'){
                                obj.position.x = v;
                            }
                            else if(prop == 'y'){
                                obj.position.y = v;
                            }
                            else if(prop == 'rotation'){
                                obj.rotation = v;
                            }
                            else if(prop == 'scalex'){
                                obj.scale.x = v;
                            }
                            else if(prop == 'scaley'){
                                obj.scale.y = v;
                            }
                            
                            if(frame_now >= tokeyframe.frame){
                                // console.log("reached",currkeyframes[prop].fromKeyframeIndex);
                                currkeyframes[prop].fromKeyframeIndex+=1;
                            }

                        }
                        // else{//last keyframe reached
                        // } 
                    } 
                }   
                
            }


            for (var i = 0; i < obj.children.length; i++) {
                o = obj.children[i];
                this.updateActions(anim, o, elaspedTime);
            }


        },
        resetfromKeyframeIndex: function(obj){
            if(obj.blen2danimation.currentAction){
                currkeyframes = obj.blen2danimation.currentAction.keyframes;
                for(var prop in currkeyframes){
                    currkeyframes[prop].fromKeyframeIndex = currkeyframes[prop].fromKeyframeIndexSaved;
                }
            }

            for (var i = 0; i < obj.children.length; i++) {
                o = obj.children[i];
                this.resetfromKeyframeIndex(o);
            }
            
        },
        update: function(){
            for (var i =0; i < this.currentAnimationList.length; i++){
                obj = this.currentAnimationList[i];
                anim = obj.blen2danimation;
                if(anim.playing){
                    elaspedTime = this.Time.now - anim.startTime;              
                    
                    loop_count = Math.floor(elaspedTime/anim.looptime);
                    if(loop_count - anim.loopcount){
                        this.resetfromKeyframeIndex(obj);
                        anim.loopcount = loop_count;
                    }
                    this.updateActions(anim, obj, elaspedTime);
                }
                    
            }
        },
        setCurrentActions: function(obj, objectName, actionName, b2danim){
            if(obj.name == objectName){
                obj.blen2danimation.currentAction = obj.blen2danimation.actions[actionName];
                // console.log(obj, obj.blen2danimation.currentAction)
                keyframes = obj.blen2danimation.currentAction.keyframes;
                for( var prop in keyframes){
                    findex = 0;
                    for(var i =0; i < keyframes[prop].datalist.length; i++){
                        if(keyframes[prop].datalist[i].frame <= b2danim.BEG){
                            findex = i;
                        }
                        else{
                            if(i==0){
                                findex = -1;
                            }
                            break;
                        }
                    }
                    keyframes[prop].fromKeyframeIndex = findex;
                    keyframes[prop].fromKeyframeIndexSaved = findex;
                    
                }
                return true;
            }

            for (var i = 0; i < obj.children.length; i++) {
                o = obj.children[i];
                if(this.setCurrentActions(o, objectName, actionName, b2danim)){
                    return true;
                }
            }

            return false;
        },
        setCurrentActionsToNone:function(obj){
            obj.blen2danimation.currentAction = null;
            for (var i = 0; i < obj.children.length; i++) {
                this.setCurrentActionsToNone(obj.children[i]);
            }
        }


    }



};

