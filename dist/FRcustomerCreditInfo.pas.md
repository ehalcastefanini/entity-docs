<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica chamado `TFRAMEcustomerCreditInfo`, que exibe informações de crédito de clientes, como status de envio, valores de pedidos, faturas, créditos disponíveis e valores vencidos. Ele é projetado para ser usado em sistemas de gestão financeira ou ERP, permitindo que os usuários visualizem e interajam com os dados financeiros de um cliente.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação usada para criar a interface gráfica e lógica do componente.
  - **Componentes Visuais:** Inclui painéis, rótulos, campos de edição vinculados a dados (DBEdit), e botões.
  - **Banco de Dados:** Utiliza `TDataSet` para manipulação de dados.
  - **SOAP:** Comunicação com serviços externos via `SOAPHTTPClient`.

* **Forma do Componente:**
  - **Formulário:** Este é um formulário que exibe informações financeiras do cliente.
    - **Elementos do Formulário e Tipos:**
      - Campos de edição vinculados a dados (`TsDBEdit` e `TsDBText`) para exibir valores financeiros.
      - Rótulos (`TsLabel`) para descrever os campos.
      - Painéis (`TsPanel`) para organizar visualmente os elementos.
    - **Ações do Formulário e Efeitos:**
      - Exibição de informações financeiras do cliente.
      - Configuração de visibilidade de painéis e ações disponíveis.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Exibir informações de crédito do cliente.
  - Configurar visibilidade de painéis e ações disponíveis.
  - Manipular o modo de acesso ao componente.

* **Componentes Principais:**
  - **Painéis (`TsPanel`):** Organizam visualmente os elementos.
  - **Campos de Edição (`TsDBEdit`):** Exibem valores financeiros vinculados ao banco de dados.
  - **Rótulos (`TsLabel`):** Descrevem os campos exibidos.
  - **Botões (`TsBitBtn`):** Permitem ações como adicionar, aplicar ou cancelar.

* **Tradução para Pseudo-código:**
  - Evento `OnSetAccessMode`: `se modo de acesso for alterado, então configurar visibilidade e ações disponíveis`.
  - Evento `CDStableAfterOpen`: `se tabela de dados for aberta, então exibir informações de crédito`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente (`Create`):
     - Configura propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Define visibilidade de painéis e ações disponíveis.
  2. Interação do Usuário:
     - O usuário visualiza informações financeiras do cliente.
     - Eventos como `OnSetAccessMode` e `CDStableAfterOpen` são disparados para atualizar a interface.

* **Dados Necessários:**
  - Código do cliente (`customerCode`).
  - Informações financeiras como pedidos, faturas, créditos disponíveis e valores vencidos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ações como "Adicionar" ou "Aplicar" só devem ser habilitadas se os dados necessários estiverem preenchidos corretamente.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`ShowCreditInfo`:** Exibe informações de crédito do cliente.
* **`m_SetAccessMode`:** Configura o modo de acesso e visibilidade de ações.
* **`CDStableAfterOpen`:** Atualiza a interface após a abertura da tabela de dados.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `DBClient`: Para manipulação de dados do cliente.

* **Componentes Customizados:**
  - `TFRAMEBaseCtrlEditSOA`: Classe base herdada para criar o componente.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `EDTshipStat` (tipo: texto, vinculado a dados).
  - `EDTordersEUR` (tipo: numérico, vinculado a dados).
  - `EDTinvoicesEUR` (tipo: numérico, vinculado a dados).
  - `EDTavailCredEUR` (tipo: numérico, vinculado a dados).
  - Outros campos similares para diferentes valores financeiros.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `EDTshipStat` mapeado para o status de envio do cliente.
  - `EDTordersEUR` mapeado para valores de pedidos em EUR.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```delphi
  constructor TFRAMEcustomerCreditInfo.Create(AOwner: TComponent);
  begin
    inherited;
    MasterKeyFields := 'customerCode';
    DataPacketName := 'CustomerCredit';
    FrameType := frtDetail;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 762px; height: 249px; font-family: Verdana;">
    <div style="padding: 10px; border: 1px solid #ccc;">
      <label>Status de Envio:</label>
      <input type="text" value="Exemplo" />
    </div>
    <div style="padding: 10px; border: 1px solid #ccc;">
      <label>Pedidos (EUR):</label>
      <input type="number" value="1000" />
    </div>
    <!-- Outros campos -->
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `// SET DAS PROPRIEDADES DA FRAME`: Configura propriedades essenciais do componente.
* `// configurar visibilidade de painel de ações e ações disponíveis`: Define a visibilidade de painéis e ações.

---

## 12. Conclusão:

O código implementa um componente visual para exibir informações financeiras de clientes. Ele é bem estruturado e utiliza componentes visuais e de dados para criar uma interface funcional. No entanto, faltam validações explícitas e mensagens de erro, o que pode limitar sua robustez.

---

## 13. Resumo Curto:

O `TFRAMEcustomerCreditInfo` é um componente Delphi que exibe informações financeiras de clientes, como status de envio, valores de pedidos e créditos disponíveis. Ele é configurável e utiliza componentes visuais e de dados para criar uma interface funcional e interativa.#### **FRcustomerCreditInfo.pas**

```
unit FRcustomerCreditInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, StdCtrls, Mask, DBCtrls,
  sDBEdit, sDBText, sLabel, sFrameAdapter, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, Buttons, sBitBtn, sPanel, sBevel;

type
  TFRAMEcustomerCreditInfo = class(TFRAMEBaseCtrlEditSOA)
    PNLshipStat: TsPanel;     //#23764 (cmosilva 10-10-2019)
    LBLshipStat: TsLabel;
    EDTshipStat: TsDBEdit;
    BVL1: TsBevel;
    PNLcurr: TsPanel;          //#23764 (cmosilva 10-10-2019)
    PNLcurrLocal1: TsPanel;    //#23764 (cmosilva 10-10-2019)
    PNLcurrForeign1: TsPanel;  //#23764 (cmosilva 10-10-2019)
    PNLlblOverdue: TsPanel;    //#23764 (cmosilva 10-10-2019)
    LBLLabel11: TsLabel;
    LBLLabel13: TsLabel;
    LBLLabel15: TsLabel;
    LBLLabel17: TsLabel;
    LBLLabel18: TsLabel;
    LBLLabel19: TsLabel;
    LBLnetOutInv: TsLabel;
    EDTordersEUR: TsDBEdit;
    EDTinvoicesEUR: TsDBEdit;
    EDToutInvoicesEUR: TsDBEdit;
    EDTworkInProgEUR: TsDBEdit;
    EDTavailCredEUR: TsDBEdit;
    EDTnetOutInvEur: TsDBEdit;
    EDTcurrencyCode: TsDBText;
    EDTordersCUR: TsDBEdit;
    EDToutInvoicesCUR: TsDBEdit;
    EDTinvoicesCUR: TsDBEdit;
    EDTworkInProgCUR: TsDBEdit;
    EDTavailCredCUR: TsDBEdit;
    EDTnetOutInvCUR: TsDBEdit;
    PNLlocalCurr2: TsPanel;     //#23764 (cmosilva 10-10-2019)
    PNLcurrForeign2: TsPanel;   //#23764 (cmosilva 10-10-2019)
    LBLov1630: TsLabel;
    LBLov3160: TsLabel;
    LBLov6190: TsLabel;
    LBLov90: TsLabel;
    LBLovTotal: TsLabel;
    LBLov115: TsLabel;
    LBLov: TsLabel;
    LBLovEurHeader: TsLabel;
    EDT1: TsDBEdit;
    EDT2: TsDBEdit;
    EDT3: TsDBEdit;
    EDT4: TsDBEdit;
    EDT5: TsDBEdit;
    EDT6: TsDBEdit;
    DBTXT1: TsDBText;
    EDToverdue1_15: TsDBEdit;
    EDToverdue15_30: TsDBEdit;
    EDToverdue31_60: TsDBEdit;
    EDToverdue61_90: TsDBEdit;
    EDToverdueM90: TsDBEdit;
    EDToverdueTotal: TsDBEdit;
    procedure CDStableAfterOpen(DataSet: TDataSet);
  private
    { Private declarations }
    procedure ShowCreditInfo;
    procedure m_SetAccessMode(Sender: TObject; var pv_stat: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerCreditInfo: TFRAMEcustomerCreditInfo;

implementation

{$R *.dfm}
uses
  kneTypes;

constructor TFRAMEcustomerCreditInfo.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerCredit';
  PropertyName := 'credit';
  FrameType := frtDetail;


  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  OnSetAccessMode := m_SetAccessMode;
```

#### **FRcustomerCreditInfo.dfm**

```
inherited FRAMEcustomerCreditInfo: TFRAMEcustomerCreditInfo
  Width = 762
  Height = 249
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 762
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

