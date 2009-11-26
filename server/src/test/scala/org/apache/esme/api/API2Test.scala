/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.esme.api 

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._ 
import org.specs.matcher._
import Helpers._
import net.sourceforge.jwebunit.junit.WebTester
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}  

import net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError                 

class Api2SpecsAsTest extends JUnit3(Api2Specs)
object Api2SpecsRunner extends ConsoleRunner(Api2Specs)

object Api2Specs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("/api2/")
  
  val theUser = User.createAndPopulate.nickname("api_test").saveMe
  val token = {
    val toke = AuthToken.create.user(theUser).saveMe
    toke.uniqueId.is
  }     

  val post_session = post("session", "token" -> token)
  
  "API2" should {
	"/session POST" in {  
	  "Attempt to log in with a valid token should succeed with a 200 response" in {
	    for{
	      session <- post_session    
	    } {
	      (session.xml \ "session" \ "user" \ "id").text must be equalTo(theUser.id.toString)
		  session.code must be equalTo 200
	    } 
	  }

	  "Attempt to create session with an invalid token returns 403 response" in {
	    for{
	      session <- post("session", "token" -> "000000")
	    } {                  
          session.code must be equalTo 403
	    } 
	  }
	}
	
	"/session GET" in {
	  "with valid session" in {
	    for {
	      session <- post_session
          session_response <- session.get("session")
	    } {
	      session_response.code must be equalTo 200
	      ( session_response.xml \ "session" \ "user" \ "id").text must be equalTo(theUser.id.toString)
	    }
	  }
	
	  "with no session returns 404 (not found)" in {
	    for(session_res <- get("session")) {
	      session_res.code must be equalTo 404
	    }
	  }  
    }

    "/session DELETE" in {
      "with valid session" in {
        for {
          session <- post_session
          session_del_response <- session.delete("session")
          //session_response <- session.get("session")
        } {
          session_del_response.code must be equalTo 200
          //session_response.code must be equalTo(404)
        }
      }

	  "with no session returns 404 (not found)" in {
	    for(session_res <- delete("session")) {
	      session_res.code must be equalTo 404
	    }
	  }
    }	

    "/users GET" in {
      "with valid session" in {     
        for {
          session <- post_session
          users <- session.get("users")
        } {
          users.code must be equalTo 200
        }
      }

	  "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("users")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages GET" in {
      "with valid session" in {
        for {
          session <- post_session
          mess_res <- session.get("user/messages")
        } {
          mess_res.code must be equalTo 200
        }
      }

	  "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages?history=10 GET" in {
      "with valid session" in {
        for {
          session <- post_session
          res <- session.get("user/messages?history=10")
        } {
          res.code must be equalTo 200
        }                     
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages?history=10")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages?timeout=2 GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/messages?timeout=2")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages?timeout=2")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/followees GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/followees")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/followees")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/followees POST" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.post("user/followees", "userId" -> "1")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- post("user/followees", "userId" -> "1")) {
	      res.code must be equalTo 403
	    }
	  }
    } 

    "/user/followers GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/followers")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/followers")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }              
  }  
}