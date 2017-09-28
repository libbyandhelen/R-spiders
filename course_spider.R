install.packages("RCurl")
install.packages("XML")
install.packages("httr")
install.packages("rjson")
install.packages("RJSONIO")
library(RCurl)
library(XML)
library(httr)
library(RJSONIO)

url = "https://cusis.cuhk.edu.hk/psc/public/EMPLOYEE/HRMS/c/COMMUNITY_ACCESS.SSS_BROWSE_CATLG.GBL"
webpage = GET(url, httpheader = c('User-Agent' = "Chrome/60.0.3112.113"))
webpage = htmlTreeParse(webpage, useInternalNodes = TRUE)

xpaths = paste("//a[@name='DERIVED_SSS_BCC_SSR_ALPHANUM_", LETTERS, "']/attribute::href", sep="")
codes = c()
#courses = c()
course_info = list()
for(i in 1:length(xpaths)){
  alphanum = getNodeSet(webpage, path=xpaths[i])
  alphanum = substr(as(alphanum, "character"), 45, 74)
  
  post_path = paste(url, "?ICAction=", alphanum, sep="")
  subject_list_page = htmlTreeParse(POST(post_path), useInternalNodes = TRUE)
  subject_list_page = htmlTreeParse(POST(post_path), useInternalNodes = TRUE)
  code = getNodeSet(subject_list_page, path="//a[@title='Show/Hide Courses for Subject']/text()")
  codes = c(codes, code)
  
  post_path = paste(url, "?ICAction=DERIVED_SSS_BCC_SSS_EXPAND_ALL$106$", sep="")
  course_list_page = htmlTreeParse(POST(post_path), useInternalNodes = TRUE)
  
  courses = getNodeSet(course_list_page, path="//a[@title='View Course Details']/attribute::href")
  if(length(courses) == 0){
    next
  }
  courses = as(courses, "character")
  lengths = sapply(courses, nchar, USE.NAMES = FALSE)
  courses = substr(courses, 45, lengths-3)
  post_paths = paste(url, "?ICAction=", courses, sep="")
  for(i in 1:length(post_paths)){
    course_detail_page = htmlTreeParse(POST(post_paths[i]), useInternalNodes = TRUE)
    title_node = getNodeSet(course_detail_page, path="//span[@class='PALEVEL0SECONDARY']/text()")
    code_and_title = strsplit(as(title_node[[1]], "character"), split=" - ", fixed=TRUE)
    course_code = code_and_title[[1]][1]
    course_title = code_and_title[[1]][2]
    
    course_detail_nodes = getNodeSet(course_detail_page, path="//span[@class='PSDROPDOWNLIST_DISPONLY']/text()")
    career = as(course_detail_nodes[[1]],"character")
    grading_basis = as(course_detail_nodes[[2]], "character")
    add_consent = NULL
    drop_consent = NULL
    drop_consent_node = getNodeSet(course_detail_page, path="//label[@for='SSR_CRSE_OFF_VW_SSR_DROP_CONSENT$0']")
    add_consent_node = getNodeSet(course_detail_page, path="//label[@for='SSR_CRSE_OFF_VW_CONSENT$0']")
    if(length(drop_consent_node)>0 && length(add_consent_node)>0){
      add_consent = as(course_detail_nodes[[3]], "character")
      drop_consent = as(course_detail_nodes[[4]], "character")
    }
    else if(length(drop_consent_node) == 0 && length(add_consent_node)>0){
      add_consent = as(course_detail_nodes[[3]], "character")
    }
    else if(length(drop_consent_node) > 0 && length(add_consent_node) == 0){
      drop_consent = as(course_detail_nodes[[3]], "character")
    }
    
    course_detail_nodes = getNodeSet(course_detail_page, path="//span[@class='PSEDITBOX_DISPONLY']/text()")
    units = as(course_detail_nodes[[1]], "character")
    components = list()
    requirement = NULL
    if(length(course_detail_nodes)%%2 == 1){
      for(j in 1:(length(course_detail_nodes)%/%2)){
        components[j] = list(component = paste(as(course_detail_nodes[[2*j]], "character"), as(course_detail_nodes[[2*j+1]], "character")))
      }
    }
    else{
      for(j in 1:(length(course_detail_nodes)%/%2-1)){
        components[j] = list(component = paste(as(course_detail_nodes[[2*j]], "character"), as(course_detail_nodes[[2*j+1]], "character")))
      }
      requirement = as(course_detail_nodes[[length(course_detail_nodes)]], "character")
    }
    
    course_detail_nodes = getNodeSet(course_detail_page, path="//span[@class='PSLONGEDITBOX']/text()")
    description = NULL
    if(length(course_detail_nodes)>1){
      description = as(course_detail_nodes[[2]], "character")
    }
    
    view_course_nodes = getNodeSet(course_detail_page, path="//a[@class='SSSBUTTON_ACTIONLINK']/attribute::href")
    view_course_nodes = as(view_course_nodes, "character")
    class_list = list()
    if(length(view_course_nodes) == 2){
      view_class_section = substr(view_course_nodes[1], 45, nchar(view_course_nodes[1])-3)
      post_path = paste(url, "?ICAction=", view_class_section, sep="")
      class_section_page = htmlTreeParse(POST(post_path), useInternalNodes = TRUE)
      
      view_all_node = getNodeSet(class_section_page, path="//td[@class='PSLEVEL1GRIDNAVIGATIONBAR']/attribute::href")
      view_all_node = as(view_all_node, "character")
      view_all_node = substr(view_all_node, 45, nchar(view_all_node)-3)
      if(length(view_all_node)!=0){
        post_path = paste(url, "?ICAction=", view_all_node, sep="")
        class_section_page = htmlTreeParse(POST(post_path), useInternalNodes = TRUE)
      }
      
      class_detail_nodes = getNodeSet(class_section_page, path="//a[@title='Class Details']/attribute::href")
      class_codes = getNodeSet(class_section_page, path="//a[@title='Class Details']/text()")
      class_detail_nodes = as(class_detail_nodes, "character")
      lengths = sapply(class_detail_nodes, nchar, USE.NAMES = FALSE)
      class_detail_nodes = substr(class_detail_nodes, 45, lengths-3)
      class_detail_paths = paste(url, "?ICAction=", class_detail_nodes, sep="")
      
      for(j in 1:length(class_detail_paths)){
        class_detail_page = htmlTreeParse(POST(class_detail_paths[j]), useInternalNodes = TRUE)
        status = NULL
        class_number = NULL
        session = NULL
        instruction_mode = NULL
        dates = NULL
        language = NULL

        class_content_nodes = getNodeSet(class_detail_page, path="//span[@class='PSEDITBOX_DISPONLY']/text()")
        status = as(class_content_nodes[[2]], "character")
        class_number = as(class_content_nodes[[3]], "character")
        session = as(class_content_nodes[[4]], "character")
        instruction_mode = as(class_content_nodes[[6]], "character")
        dates = as(class_content_nodes[[12]], "character")
        
        class_content_nodes = getNodeSet(class_detail_page, path="//span[@class='PSLONGEDITBOX']/text()")
        language = as(class_content_nodes[[length(class_content_nodes)-1]], "character")
      
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_SSR_DESCRSHORT']/parent::*/parent::*/following-sibling[1]//span/text()")
        # if(length(class_content_nodes) != 0){
        #   status = as(class_content_nodes[[1]], "character")
        # }
        # 
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_CLASS_NBR']/../../following-sibling[1]//span/text()")
        # if(length(class_content_nodes)!=0){
        #   class_number = as(class_content_nodes[[1]], "character")
        # }
        # 
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_SESSION_CODE']/../../following-sibling[1]//span/text()")
        # if(length(class_content_nodes)!=0){
        #   session = as(class_content_nodes[[1]], "character")
        # }
        # 
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_INSTRUCTION_MODE']/../../following-sibling[1]//span/text()")
        # if(length(class_content_nodes)!=0){
        #   instruction_mode = as(class_content_nodes[[1]], "character")
        # }
        # 
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_SSR_DATE_LONG']/../../following-sibling[1]//span/text()")
        # if(length(class_content_nodes)!=0){
        #   dates = as(class_content_nodes[[1]], "character")
        # }
        
        class_content_nodes = getNodeSet(class_detail_page, path="//table[@class='PSLEVEL1GRIDWBO']//span/text()")
        section_list = list()
        for(k in 1:(length(class_content_nodes)%/%4)){
          section_list[k] = list(section = list(
            days = as(class_content_nodes[[4*(k-1)+1]], "character"),
            room = as(class_content_nodes[[4*(k-1)+2]], "character"),
            instructor = as(class_content_nodes[[4*(k-1)+3]], "character"),
            meeting_dates = as(class_content_nodes[[4*(k-1)+4]], "character")
          ))
        }
        
        # class_content_nodes = getNodeSet(class_detail_page, path="//label[@for='SSR_CLS_DTL_WRK_SSR_CRSE_ATTR_LONG']/../following-sibling[1]//span/text()")
        # if(length(class_content_nodes) != 0){
        #   language = as(class_content_nodes[[1]], "character")
        # }
        # 
        class_content_nodes = getNodeSet(class_detail_page, path="//td[contains(text(), 'Class Availability')]/../..//span/text()")
        if(length(class_content_nodes)!=0){
          class_capacity = as(class_content_nodes[[1]], "character")
          wait_list_capacity = as(class_content_nodes[[2]], "character")
          enrollment_total = as(class_content_nodes[[3]], "character")
          wait_list_total = as(class_content_nodes[[4]], "character")
          available_seat = as(class_content_nodes[[5]], "character")
        }
        
        class_list[j] = list(class_list = list(
          class_code = as(class_codes[[j]], "character"),
          status = status,
          class_number = class_number,
          session = session,
          instruction_mode = instruction_mode,
          dates = dates,
          language = language,
          class_section = section_list
        ))
        
        POST(paste(url, "?ICAction=CLASS_SRCH_WRK2_SSR_PB_CLOSE", sep=''))
      }
    }
    
    course_outcome_nodes = getNodeSet(course_detail_page, path="//a[@name='CU_DERIVED_CUR_CU_CRSE_OUT_BTN']/attribute::href")
    course_outcome_nodes = as(course_outcome_nodes, "character")
    course_outcome_path = substr(course_outcome_nodes, 45, nchar(course_outcome_nodes)-3)
    
    course_outcome_path = paste(url, "?ICAction=", course_outcome_path, sep="")
    course_outcome_page = htmlTreeParse(POST(course_outcome_path), useInternalNodes = TRUE)
    
    course_outcome_content = getNodeSet(course_outcome_page, path="//table[@class='PSGROUPBOX']//div[text()!='']")
    if(length(course_outcome_content) == 6){
      learning_outcome = xmlValue(course_outcome_content[[2]])
      syllabus = xmlValue(course_outcome_content[[3]])
      feedback = xmlValue(course_outcome_content[[4]])
      required_reading = xmlValue(course_outcome_content[[5]])
      recommended_reading = xmlValue(course_outcome_content[[6]])
    }
    else{
      learning_outcome = xmlValue(course_outcome_content[[1]])
      syllabus = xmlValue(course_outcome_content[[2]])
      feedback = xmlValue(course_outcome_content[[3]])
      required_reading = xmlValue(course_outcome_content[[4]])
      recommended_reading = xmlValue(course_outcome_content[[5]])
    }
    
    course_outcome_content = getNodeSet(course_outcome_page, path="//span[@class='PSEDITBOX_DISPONLY']/text()")
    assessment = list()
    for(j in 1:(length(course_outcome_content)%/%2)){
      type = as(course_outcome_content[[2*j-1]], "character")
      percent = as(course_outcome_content[[2*j]], "character")
      assessment[j] = list(assessment = list(
        type = type,
        percent = percent
      ))
    }
    
    course_info[i] = (list(
      course = list(
        course_code = course_code,
        course_title = course_title,
        add_consent = add_consent,
        drop_consent = drop_consent,
        career = career,
        grading_basis = grading_basis,
        units = units,
        components = components,
        requirement = requirement,
        description = description,
        learning_outcome = learning_outcome,
        syllabus = syllabus,
        assessment = assessment,
        feedback = feedback,
        required_reading = required_reading,
        recommended_reading = recommended_reading,
        class_list = class_list
      )
    ))
    POST(paste(url, "?ICAction=DERIVED_SAA_CRS_RETURN_PB", sep=''))
    print(paste(course_code, course_title))
  }
  
}

b = toJSON(course_info, pretty = TRUE)
write(b, "/Users/yingbozhang/Desktop/rproj/test.json")
