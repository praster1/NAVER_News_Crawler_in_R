### 오리지날 코드 경로: https://dr-hkim.github.io/Naver-News-Web-Scraping-using-Keywords-in-R/

news_crawling = function(keyword, startDate, endDate, print=TRUE)
{
	## 변수 입력하기
	QUERY  =  keyword			# 검색키워드
	DATE   =  as.Date(as.character(startDate),format="%Y%m%d")		# 검색시작날짜 & 검색종료날짜
	DATE  =  format(DATE, "%Y.%m.%d")
	PAGE   =  1

	naver_url_1  =  "https://search.naver.com/search.naver?&where=news&query="
	naver_url_2  =  "&pd=3&ds="
	naver_url_3  =  "&de="
	naver_url_4  =  "&start="

	### 날짜 리스트 만들기
	DATE_START  =  as.Date(as.character(startDate),format="%Y%m%d")		# 시작일자
	DATE_END    =  as.Date(as.character(endDate),  format="%Y%m%d")		# 종료일자
	DATE  =  DATE_START:DATE_END
	DATE  =  as.Date(DATE,origin="1970-01-01")

	### 게시물 번호 리스트 만들기
	PAGE  =  seq(from=1,to=41,by=10)					# 시작값과 종료값을 지정해줄 수 있습니다.
	PAGE  =  seq(from=1,by=10,length.out=5)		# 시작값과 원하는 갯수를 지정할 수도 있습니다.

	### 네이버 검색결과 url 리스트에서 관련기사 url 리스트 만들기
	
	tempFunc_date = function(date_i)
	{
		tempFunc_page = function(page_i)
		{
			dt  =  format(as.Date(date_i,origin="1970-01-01"), "%Y.%m.%d")
			naver_url  =  paste0(naver_url_1,QUERY,naver_url_2,dt,naver_url_3,dt,naver_url_4,page_i)
			html  =  read_html(naver_url)
			temp  =  unique(html_nodes(html,'#main_pack')%>%		# id= 는 # 을 붙인다
						 html_nodes(css='.news ')%>%							# class= 는 css= 를 붙인다 
						 html_nodes(css='.type01')%>%
						 html_nodes('a')%>%
						 html_attr('href'))
			news_url  =  temp
			news_date  =  rep(dt,length(temp))
			
			return(list(newsURL = news_url, newsDate = news_date))
		}
		
		temp = pbmclapply(PAGE, tempFunc_page, mc.cores=numCores)
		return(temp)
	}
	
	temp = pbmclapply(DATE, tempFunc_date, mc.cores=numCores)
	
	getNewsURL = function(i, temp)
	{
		tempFunc = function(j, temp1)	{	return(temp1[[j]]$newsURL)	}
		return(unlist(pbmclapply(1:length(temp[[i]]), tempFunc, temp[[i]], mc.cores=numCores)))
	}
	
	getNewsDate = function(i, temp)
	{
		tempFunc = function(j, temp1)	{	return(temp1[[j]]$newsDate)	}
		return(unlist(pbmclapply(1:length(temp[[i]]), tempFunc, temp[[i]], mc.cores=numCores)))
	}
	
	news_url = unlist(pbmclapply(1:length(temp), getNewsURL, temp, mc.cores=numCores))
	news_date = unlist(pbmclapply(1:length(temp), getNewsDate, temp, mc.cores=numCores))
	
	
	NEWS0  =  as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
	NEWS1  =  NEWS0[which(grepl("news.naver.com",NEWS0$url)),]  		      	 # 네이버뉴스(news.naver.com)만 대상으로 한다   
	NEWS1  =  NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),]		 # 스포츠뉴스(sports.news.naver.com)는 제외한다  
	NEWS2  =  NEWS1[!duplicated(NEWS1), ] 		# 중복된 링크 제거 


	### 뉴스 페이지에 있는 기사의 제목과 본문을 크롤링
	NEWS2$news_title    =  ""
	NEWS2$news_content  =  ""

	# for (i in 1:dim(NEWS2)[1])
	tempFunc_NEWS2 = function(i, NEWS2)
	{
		html  =  read_html(as.character(NEWS2$url[i]))
		temp_news_title    =  repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
		temp_news_content  =  repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
		if (length(temp_news_title)>0)
		{
			NEWS2$news_title[i]    =  temp_news_title
			NEWS2$news_content[i]  =  temp_news_content
		}
		
		return(NEWS2)
	}
	
	temp = pbmclapply(1:dim(NEWS2)[1], tempFunc_NEWS2, NEWS2, mc.cores=numCores)
	
	getMatrix = function(i, temp)
	{
		return(cbind(as.character(temp[[i]]$date), as.character(temp[[i]]$url), as.character(temp[[i]]$query), as.character(temp[[i]]$news_title), as.character(temp[[i]]$news_content)))
	}
	
	temp3 = pbmclapply(1:length(temp), getMatrix, temp, mc.cores=numCores)
	
	NEWS2 = do.call(rbind, temp3)
	colnames(NEWS2) = c("date", "url", "query", "title", "content")
	NEWS2[,5] = gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2[,5])
	
	return(NEWS2)
}


startDate = 20170101
endDate = 20171231
inputWord = "키워드"			# 2018~2019년 뉴스는 다시 크롤링할 필요가 있음.

data = news_crawling(keyword=inputWord, startDate=startDate, endDate=endDate, print=TRUE)