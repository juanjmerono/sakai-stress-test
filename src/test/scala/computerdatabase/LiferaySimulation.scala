
import scala.concurrent.duration._

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.jdbc.Predef._
import io.gatling.core.action.builder._
import java.net._
import scala.collection.mutable.ListBuffer
import liferay.MyEncryptor

class LiferaySimulation extends Simulation {

	val successStatus: Int = 200
	val pauseMin: Int = Integer.getInteger("min-pause",1)
	val pauseMax: Int = Integer.getInteger("max-pause",1)
	val randomUsers: Int = Integer.getInteger("random-users",1)
	val exhausUsers: Int = Integer.getInteger("exhaus-users",1)
	val rampUpTime: Int = Integer.getInteger("rampup-time",10)
	val rampUpRandomUsers: Int = Integer.getInteger("rampup-random-users",0)
	val rampUpExhausUsers: Int = Integer.getInteger("rampup-exhaus-users",0)
	val rampUpWait: Int = Integer.getInteger("rampup-wait",10)
	val siteLoop: Int = Integer.getInteger("site-loop",1)
	val toolLoop: Int = Integer.getInteger("tool-loop",1)
	val userLoop: Int = Integer.getInteger("user-loop",1)
	val privatePrefix = System.getProperty("private-prefix")
	val fixedSiteId = System.getProperty("fixed-site")
	val fixedToolId = System.getProperty("fixed-tool")
	val fixedSiteTitle = System.getProperty("fixed-site-title")
	
	val httpProtocol = http.disableWarmUp
		.baseURL(System.getProperty("test-url"))
		/**.inferHtmlResources(BlackList(".*(\.css|\.js|\.png|\.jpg|\.gif|thumb).*"), WhiteList())*/
		.userAgentHeader("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36")
		.disableCaching
		/** Extra info for simulation.log username, url, http status, response length */
		.extraInfoExtractor(extraInfo => List(extraInfo.session("username").asOption[String].getOrElse(extraInfo.session("adminusername").asOption[String].getOrElse("annonymous")),
											  extraInfo.request.getUrl,
											  extraInfo.response.statusCode.getOrElse(0),
											  extraInfo.response.bodyLength))

	val headers = Map(
		"Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
		"Accept-Encoding" -> "gzip, deflate, sdch, br",
		"Accept-Language" -> "es-ES,es;q=0.8,en;q=0.6",
		"Cache-Control" -> "max-age=0",
		"Connection" -> "keep-alive",
		"Upgrade-Insecure-Requests" -> "1")

	/** Let change feed strategy and avoid error if there are not enough users in the feed */
	def getFeeder(name: String, strategy: String) = strategy match {
		case "queue" => csv(name).queue
		case "shuffle" => csv(name).shuffle
		case "random" => csv(name).random
		case "circular" => csv(name).circular
		case whatever => csv(name) /** queue is default strategy */
	} 

	val prefix = if (privatePrefix == "true") "private_liferay_" else ""
	val users = getFeeder(prefix+"user_credentials.csv",System.getProperty("feed-strategy"))
	val admins = getFeeder(prefix+"admin_credentials.csv",System.getProperty("feed-strategy"))
	
	def join(first: Vector[String], second: Vector[String]) : Vector[(String,String)] = (first zip second)
	def checkAttrs(cssSelector: String, attrName: String, varName: String) = css(cssSelector,attrName).findAll.saveAs(varName)
	def checkElement(cssSelector: String, varName: String) = css(cssSelector).findAll.saveAs(varName)
	
	def checkItsMe (username: String) = 
		css("em.useremail:contains("+username+")").exists
	
	def joinInSessionOneFiltered(session: Session, firstName: String, secondName: String, finalName: String, filteredBy: String) = 
		session
		.remove(firstName)
		.remove(secondName)
		.set(finalName, join(session(firstName).as[Vector[String]],session(secondName).as[Vector[String]]).filter(_._1 contains filteredBy)(0))

	def joinInSession(session: Session, firstName: String, secondName: String, finalName: String) = 
		session
		.remove(firstName)
		.remove(secondName)
		.set(finalName, util.Random.shuffle(join(session(firstName).as[Vector[String]],session(secondName).as[Vector[String]])))

	def joinInSessionFiltered(session: Session, firstName: String, secondName: String, finalName: String, oneFilteredBy: String, twoFilteredBy: String) = 
		session
		.remove(firstName)
		.remove(secondName)
		.set(finalName, join(session(firstName).as[Vector[String]],session(secondName).as[Vector[String]]).filter(_._1 matches oneFilteredBy).filter(_._2 matches twoFilteredBy))
		
	def clearAjaxData(session: Session, var1: String, var2: String) =
		session
		.remove(var1)
		.remove(var2)
		
	object Gateway {
		val gateway = group("Gateway") {
			exec(http("Portal")
				.get("/web/suma/")
				.headers(headers)
				.check(status.is(successStatus)))
			.pause(pauseMin,pauseMax)
		}
	}

	object Login {
		val login = (impersonate: Boolean) =>
		group("Login") {
			doIfOrElse(impersonate) { /** Login as admin and then impersonate user */
				feed(admins)
				.exec(http("Login")
					.post("/web/suma/login")
					.headers(headers)
					.formParam("p_p_id", "58")
					.formParam("p_p_lifecycle", "1")
					.formParam("p_p_state", "normal")
					.formParam("p_p_mode", "view")
					.formParam("p_p_col_id", "column-2")
					.formParam("p_p_col_count", "1")
					.formParam("_58_struts_action", "/login/login")
					.formParam("_58_login", "${adminusername}")
					.formParam("_58_password", "${adminpassword}")
					.formParam("_58_formDate", "1512032702585")
					.formParam("_58_saveLastPath", "false")
					.formParam("_58_redirect", "")
					.formParam("_58_doActionAfterLogin", "false")
					.check(status.is(successStatus))
					.check(checkItsMe("${adminusername}")))
				.pause(pauseMin,pauseMax)
				.group("ImpersonateUser") {
					exec(http("Company")
						.get("/api/jsonws/company/get-company-by-virtual-host/virtual-host/www.um.es")
						.headers(headers)
						.check(status.is(successStatus))
						.check(jsonPath("$.companyId").optional.saveAs("companyId")))
					.pause(pauseMin,pauseMax)
					.feed(users)
					.exec(http("User")
						.get("/api/jsonws/user/get-user-by-email-address/company-id/${companyId}/email-address/${username}")
						.headers(headers)
						.check(status.is(successStatus))
						.check(jsonPath("$.userId").optional.saveAs("userId")))
					.exec(session => session.set("doAsUserId","?doAsUserId="+URLDecoder.decode(MyEncryptor.encrypt(System.getProperty("liferay-key"),session("userId").as[String]),"UTF-8")))
					.exec(http("Impersonate")
						.get("/web/suma/inicio${doAsUserId}")
						.headers(headers)
						.check(checkItsMe("${username}")))
				}
			}
			{	/** Use real credentials */
				feed(users)
				.exec(http("Login")
					.get("/web/suma/login")
					.headers(headers)
					.check(status.is(successStatus)))
				.pause(pauseMin,pauseMax)
				.exec(http("PostLogin")
					.post("/web/suma/login")
					.headers(headers)
					.formParam("p_p_id", "58")
					.formParam("p_p_lifecycle", "1")
					.formParam("p_p_state", "normal")
					.formParam("p_p_mode", "view")
					.formParam("p_p_col_id", "column-2")
					.formParam("p_p_col_count", "1")
					.formParam("_58_struts_action", "/login/login")
					.formParam("_58_login", "${username}")
					.formParam("_58_password", "${password}")
					.formParam("_58_formDate", "0")
					.formParam("_58_saveLastPath", "false")
					.formParam("_58_redirect", "")
					.formParam("_58_doActionAfterLogin", "false")
					.check(status.is(successStatus))
					.check(checkItsMe("${username}")))
				.pause(pauseMin,pauseMax)
				exec(session => session.set("doAsUserId",""))
			}
		} 
	}

	object ExploreTool {
		val explore =
			group("${tool._1}") {
				exec(http("${tool._1}")
					.get("${tool._2}")
					.headers(headers)
					.check(status.is(successStatus))
					.check(checkItsMe("${username}"))
					.check(css("nav#breadcrumbs li:last a","href").is("${tool._2}"))
					.check(css("form[action^='/delegate/proxy/']","action").findAll.optional.saveAs("frameUrls"))
					.check(css("form[action^='/delegate/proxy/'] > input[name='sessionid']","value").findAll.optional.saveAs("frameIds"))
					.check(css("form[action^='/delegate/proxy/'] > input[name='urlpci']","value").findAll.optional.saveAs("frameNames")))
				.pause(pauseMin,pauseMax)
				/** Take care of all iframed tools */
				.doIf("${frameUrls.exists()}") {
					exec(session => { joinInSession(session,"frameNames","frameUrls","frames") })
					.exec(session => session.set("pcisession",session("frameIds").as[Vector[String]].lift(0).get) )
					.foreach("${frames}","frame") {
						group("generico_tp.entrada_por_suma") {
							exec(http("${frame._1}")
								.post("${frame._2}")
								.headers(headers)
								.formParam("urlpci", "${frame._1}")
								.formParam("sessionid", "${pcisession}")
								.check(regex("window.location.href=\"\\s*([^\"\\s]*)\\s*\"").optional.saveAs("menuurl"))
								.check(regex("window.location.href=\"\\s*([^\"\\s]*)\\s*\"").optional.saveAs("menupci"))
								.check(status.is(successStatus)))
							.pause(pauseMin,pauseMax)
							.asLongAs(session => session("menupci").as[String]!="END") {
								exec(session => session.set("menupci","END"))
								.exec(http("${menuurl}")
									.get("/delegate/proxy/pci_pci/${menuurl}")
									.headers(headers)
									.check(regex("window.location.href=\"\\s*([^\"\\s]*)\\s*\"").optional.saveAs("menuurl"))
									.check(regex("window.location.href=\"\\s*([^\"\\s]*)\\s*\"").optional.saveAs("menupci"))
									.check(status.is(successStatus)))
								.pause(pauseMin,pauseMax)
							}
							.exec(http("${menuurl}")
								.get("/delegate/proxy/pci_pci/${menuurl}")
								.headers(headers)
								.check(css("link[href='/WWW/estilos/estilo_suma.css']").exists)
								.check(status.is(successStatus)))
							.exec(session => session.remove("frameIds").remove("pcisession").remove("menupci").remove("menuurl"))
						}
					}
				}
				.pause(pauseMin,pauseMax)
			}
		
	}

	object BrowseTools {
		val browse = (random: Boolean) =>
			group("Tools") {
				doIfOrElse(random) {
					repeat(toolLoop) {
						exec(session => { 
							session.set("tool",session("tools").as[Vector[String]].lift(util.Random.nextInt(session("tools").as[Vector[String]].size)).get)
						})
						.exec(ExploreTool.explore)
					}
					 
				}
				{
					foreach("${tools}","tool") {
						ExploreTool.explore
					}
				}
			}
	}

	object ExploreSite {
		val explore = (random: Boolean) =>
			group("GetSite") {
				exec(http("GetSite")
					.get("/web/suma/inicio${doAsUserId}")
					.headers(headers)
					.check(status.is(successStatus))
					.check(checkItsMe("${username}"))
					.check(checkAttrs("nav[role='navigation'] ul[role='menubar'] a[href^='"+System.getProperty("test-url")+"/web/suma/']","href","toolUrls"))
					.check(css("nav[role='navigation'] ul[role='menubar'] a[href^='"+System.getProperty("test-url")+"/web/suma/']","href").findAll.transform(
						full_list => {
							/** reduce links to ids */
							val new_list = new Array[String](full_list.length) 
							for (i <- 0 until full_list.length) {
								new_list(i) = full_list(i).replace(System.getProperty("test-url")+"/web","").replaceAll("\\?doAsUserId=.*","").trim()
							}
							new_list.to[collection.immutable.Seq]
						}).saveAs("toolIds")))
				.pause(pauseMin,pauseMax)
				.exec(session => { joinInSessionFiltered(session,"toolIds","toolUrls","tools",fixedToolId,".*\\/web\\/suma\\/.*") })
			}
			.doIf(_("tools").as[Vector[String]].length>0) {
				BrowseTools.browse(random)
			}
		
	}

	object Logout {
		val logout = group("Logout") {
			exec(http("Logout")
				.get("/c/portal/logout")
				.headers(headers)
				.check(status.is(successStatus)))
		}
	}
	
	object LiferaySimulationSteps {
		val test = (random: Boolean, impersonate: Boolean) => repeat(userLoop) {
			exec(
				Gateway.gateway,
				Login.login(impersonate),
				repeat(siteLoop) {
					ExploreSite.explore(random)
				},
				Logout.logout)
		}
	}
	
	val impersonateUsers = System.getProperty("impersonate-users") == "true"
	val randomUsersScn = scenario("LiferayRandomUserSimulation").exec(LiferaySimulationSteps.test(true,impersonateUsers))
	val exhaustiveUsersScn = scenario("LiferayExhaustiveUserSimulation").exec(LiferaySimulationSteps.test(false,impersonateUsers))

	object Setup {
		val scenario = ListBuffer[io.gatling.core.structure.PopulationBuilder]()
		if (randomUsers>0) {
			if (rampUpRandomUsers>0) {
				if (rampUpRandomUsers<randomUsers) { 
					scenario += randomUsersScn.inject(
					    splitUsers(randomUsers) into(rampUsers(rampUpRandomUsers) over(rampUpTime seconds)) separatedBy(rampUpWait seconds)
					    /** More options here http://gatling.io/docs/2.2.2/general/simulation_setup.html */
					)
				} else {
					throw new IllegalArgumentException("ERROR: rampUpRandomUsers must be lower than randomUsers !!")
				}
			} else {
				scenario += randomUsersScn.inject(
				    rampUsers(randomUsers) over (rampUpTime seconds)
				    /** More options here http://gatling.io/docs/2.2.2/general/simulation_setup.html */
				)
			}
		}
		if (exhausUsers>0) {
			if (rampUpExhausUsers>0) {
				if (rampUpExhausUsers<exhausUsers) {
					scenario += exhaustiveUsersScn.inject(
					    splitUsers(exhausUsers) into(rampUsers(rampUpExhausUsers) over(rampUpTime seconds)) separatedBy(rampUpWait seconds)
					    /** More options here http://gatling.io/docs/2.2.2/general/simulation_setup.html */
					)
				} else {
					throw new IllegalArgumentException("ERROR: rampUpExhausUsers must be lower than exhausUsers !!")
				}
			} else {
				scenario += exhaustiveUsersScn.inject(
				    rampUsers(exhausUsers) over (rampUpTime seconds)
				    /** More options here http://gatling.io/docs/2.2.2/general/simulation_setup.html */
				)
			}
		}
	}

	setUp(Setup.scenario.toList).protocols(httpProtocol)
}