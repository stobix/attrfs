-ifdef(debug).
-define(DEB1(X),io:format("~p:~p ~p~n",[?MODULE,?LINE,X])).
-define(DEB2(X,Y),io:format("~p:~p "++X,[?MODULE|[?LINE|Y]])).
-else.
-define(DEB1(X),void).
-define(DEB2(X,Y),void).
-endif.

% Filsystem verkar fungera primärt med inodnummer och sekundärt med namn. nummer har namn, inte tvärt om. Får bygga om mitt katalogträd.

% Bäst kanske vore en struktur där jag både har kataloger med filer i som har inoder, och en lista med inoder med tillhörande kataloger? Verkar rätt bäst.

-record(file,{inode,name,owner,group,rights,ctime,atime,mtime})

-record(state,{dir_structure,inode_list})
